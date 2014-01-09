;; -*- lexical-binding: t; -*-

(require 'ede-compdb)
(require 'ert)

(defvar ede-compdb-test-srcdir
  (file-name-as-directory 
   (expand-file-name "test" (when load-file-name (file-name-directory load-file-name)))))

(defun temp-directory-fixture (body)
  "Calls BODY with a temporary directory. This directory will be deleted on exit/error."
  (let ((builddir (make-temp-file "build-" t)))
    (unwind-protect
        (funcall body (file-name-as-directory builddir))
      (progn
        (delete-directory builddir t)
        (ede-flush-deleted-projects)))))

(defun invoke-cmake (srcdir builddir &rest args)
  "Invokes cmake on the SRCDIR to build into BUILDDIR"
  (erase-buffer)
  (let* ((default-directory builddir)
         (ret (apply 'call-process (append '("cmake" nil t t "-DCMAKE_EXPORT_COMPILE_COMMANDS=1") args (list srcdir)))))
    (when (> 0 ret)
      (error "Error running CMake: error %d" ret)))
  )
  
(defun cmake-build-directory-fixture (body)
  "Runs cmake in a temporary build directory"
  (temp-directory-fixture
   (lambda (builddir)
     (should (file-exists-p builddir))
     (with-current-buffer (get-buffer-create "*ede-compdb-test*")
       (invoke-cmake ede-compdb-test-srcdir builddir)
       (funcall body ede-compdb-test-srcdir builddir)
       ))
   ))

(defun sleep-until-compilation-done ()
  (let* ((comp-buf (get-buffer "*compilation*"))
         (comp-proc (get-buffer-process comp-buf)))
    (while comp-proc
      (sleep-for 1)
      (setq comp-proc (get-buffer-process comp-buf)))))


(ert-deftest parse-command-line ()
  "Tests parsing of command lines"
  (let ((savedcache ede-compdb-compiler-cache)
        (f nil))
    (unwind-protect
        (progn
          ;; Prepopulate the compiler cache so that we know what to expect in it
          (setq ede-compdb-compiler-cache '(("clang" . "/opt/clang/include")))
          (setq f (compdb-entry "foo.cpp" :command-line
                                "clang -Dfoo -Dbar=baz -Uqux -I/opt/local/include -Iincludes -include bar.hpp main.cpp"))

          (should (equal "clang" (oref f compiler)))
          (should (equal '(("foo") ("bar" . "baz")) (oref f defines)))
          (should (equal '("qux") (oref f undefines)))
          (should (equal '("/opt/local/include" "includes" "/opt/clang/include") (oref f include-path)))
          (should (equal '("bar.hpp") (oref f includes)))
          )
      (setq ede-compdb-compiler-cache savedcache))))

(ert-deftest open-file-parsed ()
  "Tests the parsing of a source file. We ensure it correctly locates all include files."
  ;;:expected-result :passed ;; TODO failed if we can't locate cmake on the path
  (cmake-build-directory-fixture
   (lambda (testdir builddir)
     (let ((proj (ede-add-project-to-global-list
                  (ede-compdb-project "TESTPROJ" :file (expand-file-name "compile_commands.json" builddir))))
           (hellocpp (expand-file-name "hello.cpp" testdir))
           (confighpp (expand-file-name "config.hpp" testdir)))

       ;; Basic sanity checks on the project itself
       (should (eq proj (ede-directory-get-open-project testdir)))
       (should (gethash (file-truename hellocpp) (oref proj compdb)))

       ;; Now we'll open a source file and check that it is parsed correctly
       (let ((buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (eq proj (ede-current-project)))
               (should (oref ede-object compilation))

               ;; Should have been parsed
               (should (semantic-active-p))
               (should (not semantic-parser-warnings))

               (let* ((tags (semantic-fetch-tags))
                      (includes (semantic-find-tags-included tags))
                      (funcs (semantic-find-tags-by-class 'function tags)))
                 ;; All includes should be parsed
                 (should includes)
                 (dolist (inc includes)
                   (should (semantic-dependency-tag-file inc)))

                 ;; These function names are defined using macros, so shouldn't be visible unless we
                 ;; have parsed the preprocessor map correctly
                 ;; (should (semantic-find-tags-by-name "HelloFoo" funcs))
                 ;; (should (semantic-find-tags-by-name "HelloBar" funcs))
                 ;; (should (semantic-find-tags-by-name "HelloBaz" funcs))
               ))
           (kill-buffer buf)))

       ;; Try a header file
       (let ((buf (find-file-noselect confighpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (eq proj (ede-current-project)))
               (should (not (oref ede-object compilation)))
               )
           (kill-buffer buf)))
       ))))

(ert-deftest compiler-include-path-cache ()
  "Tests that the compiler include paths are detected."
  (cmake-build-directory-fixture
   (lambda (testdir builddir)
     (let ((savedcache ede-compdb-compiler-cache)
           (proj nil))
       (unwind-protect
           (progn
             (setq ede-compdb-compiler-cache nil)
             (setq proj (ede-add-project-to-global-list 
                         (ede-compdb-project "TESTPROJ" :file (expand-file-name "compile_commands.json" builddir))))
             (should (listp ede-compdb-compiler-cache))
             
             ;; Check that compiler includes are present in the project includes
             (when (car ede-compdb-compiler-cache)
               (let* ((hellocpp (expand-file-name "hello.cpp" testdir))
                      (buf (find-file-noselect hellocpp)))
                 (unwind-protect
                     (let* ((target (ede-find-target proj buf))
                            (entry (oref target compilation))
                            (compiler (oref entry compiler)))
                       (should (memq (cdr (assoc compiler ede-compdb-compiler-cache)) (oref entry include-path))))
                   (kill-buffer buf))))
             )
         (setq ede-compdb-compiler-cache savedcache)))
     )))

(ert-deftest multiple-configuration-directories ()
  "Tests that we can track multiple configuration directories. We create two projects, Debug and Release, and check that they can both build"
  (temp-directory-fixture
   (lambda (builddir)
     (let ((dbgdir (file-name-as-directory (concat builddir "debug")))
           (reldir (file-name-as-directory (concat builddir "release"))))
       (make-directory dbgdir)
       (make-directory reldir)

       (with-temp-buffer
         (invoke-cmake ede-compdb-test-srcdir dbgdir "-DCMAKE_BUILD_TYPE=Debug")
         (invoke-cmake ede-compdb-test-srcdir reldir "-DCMAKE_BUILD_TYPE=Release")

         (let ((proj (ede-compdb-project
                      "TESTPROJ"
                      :configuration-directories (list dbgdir reldir))))

           (should (file-exists-p dbgdir))
           (project-compile-project proj)
           (sleep-until-compilation-done)
           (should (file-executable-p (concat dbgdir "hello")))
           
           (oset proj configuration-default "release")
           (should (file-directory-p reldir))
           (project-compile-project proj)
           (sleep-until-compilation-done)
           (should (file-executable-p (concat reldir "hello")))
         
           ))))))

(ert-deftest autoload-project ()
  "Tests that we can autoload a project depending on the presence of a compilation database file"
  (temp-directory-fixture
   (lambda (builddir)
     ;; To keep the source directory clean, we'll copy the test project into the temp directory
     (copy-directory ede-compdb-test-srcdir builddir)

     (let ((srcdir (file-name-as-directory (concat builddir "test"))))
       (invoke-cmake "." srcdir)

       (let* ((hellocpp (expand-file-name "hello.cpp" srcdir))
              (buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (ede-current-project))
               (should ede-object)
               (should (oref ede-object compilation))
               )
           (kill-buffer buf))
         )))))
