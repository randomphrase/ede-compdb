;; ede-compdb-test.el --- Tests for ede-compdb.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development ninja build cedet ede

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the accompanying readme.org for quickstart and usage information

;;; Code:

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

(defun invoke-cmake (gencompdb srcdir builddir &rest args)
  "Invokes cmake on the SRCDIR to build into BUILDDIR with
ARGS. If GENCOMPDB is non-nill, a compilation database will be
generated"
  (erase-buffer)
  (let* ((default-directory builddir)
         (ret (apply 'call-process (append '("cmake" nil t t)
                                           (when gencompdb (list "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"))
                                           args (list srcdir)))))
    (when (> 0 ret)
      (error "Error running CMake: error %d" ret)))
  )
  
(defun cmake-build-directory-fixture (gencompdb body &rest args)
  "Runs cmake in a temporary build directory"
  (temp-directory-fixture
   (lambda (builddir)
     (should (file-exists-p builddir))
     (with-current-buffer (get-buffer-create "*ede-compdb-test*")
       (apply 'invoke-cmake (append (list gencompdb ede-compdb-test-srcdir builddir) args))
       (funcall body ede-compdb-test-srcdir builddir)
       ))
   ))

(defun sleep-until-compilation-done ()
  (let* ((comp-buf (get-buffer "*compilation*"))
         (comp-proc (get-buffer-process comp-buf)))
    (while comp-proc
      (sleep-for 1)
      (setq comp-proc (get-buffer-process comp-buf)))))

(defun insource-build-fixture (gencompdb body &rest args)
  "Sets up a source tree in a temporary directory for an
in-source build"
  (temp-directory-fixture
   (lambda (builddir)
     ;; To keep the source directory clean, we'll copy the test project into the temp directory
     (copy-directory ede-compdb-test-srcdir builddir)
     (let ((srcdir (file-name-as-directory (concat builddir "test"))))
       (apply 'invoke-cmake (append (list gencompdb "." srcdir) args))
       (funcall body srcdir)
       ))
   ))



(ert-deftest parse-command-line ()
  "Tests parsing of command lines"
  (let ((savedcache ede-compdb-compiler-cache)
        (f nil))
    (unwind-protect
        (progn
          ;; Prepopulate the compiler cache so that we know what to expect in it
          (setq ede-compdb-compiler-cache '(("g++" . "/opt/gcc/include")))
          (setq f (compdb-entry "foo.cpp" :command-line
                                "g++ -Dfoo -Dbar=baz -Uqux -isystem /opt/quxx/include -I/opt/local/include -Iincludes -include bar.hpp main.cpp"))

          (parse-command-line-if-needed f)
          (should (equal "g++" (oref f compiler)))
          (should (equal '(("foo") ("bar" . "baz")) (oref f defines)))
          (should (equal '("qux") (oref f undefines)))
          (should (equal '("/opt/quxx/include" "/opt/local/include" "includes" "/opt/gcc/include") (oref f include-path)))
          (should (equal '("bar.hpp") (oref f includes)))
          )
      (setq ede-compdb-compiler-cache savedcache))))

(ert-deftest empty-build-dir ()
  "Tests that we can still open files when the build directory can't be located, or is empty"
  (temp-directory-fixture
   (lambda (tmpdir)
     (let* ((srcdir ede-compdb-test-srcdir)
            (proj (ede-add-project-to-global-list
                  (ede-compdb-project "TESTPROJ"
                                      :compdb-file (expand-file-name "compile_commands.json" tmpdir)
                                      :file (expand-file-name "CMakeLists.txt" srcdir))))
            (hellocpp (expand-file-name "hello.cpp" srcdir)))

       (should (eq proj (ede-directory-get-open-project srcdir)))

       (let ((buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (eq proj (ede-current-project)))
               (should (not (oref ede-object compilation)))

               ;; Should have been parsed
               (should (semantic-active-p))
               )
         (kill-buffer buf)))
       ))))

(ert-deftest open-file-parsed ()
  "Tests the parsing of source files in a project. We ensure it correctly locates all include files, amongst other things."
  ;;:expected-result :passed ;; TODO failed if we can't locate cmake on the path
  (cmake-build-directory-fixture t
   (lambda (testdir builddir)
     (let ((proj (ede-add-project-to-global-list
                  (ede-compdb-project "TESTPROJ"
                                      :compdb-file (expand-file-name "compile_commands.json" builddir)
                                      :file (expand-file-name "CMakeLists.txt" testdir))))
           (hellocpp (expand-file-name "hello.cpp" testdir))
           (worldcpp (expand-file-name "world/world.cpp" testdir))
           (testbufs nil))

       ;; Basic sanity checks on the project itself
       (should (eq proj (ede-directory-get-open-project testdir)))
       (should (gethash (file-truename hellocpp) (oref proj compdb)))

       ;; Now we'll open source files and check that they are parsed correctly
       (unwind-protect
           (progn
             (let ((buf (find-file-noselect hellocpp)))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                 ;; Should have set up the current project and target
                 (should (eq proj (ede-current-project)))
                 (should (oref ede-object compilation))
                 
                 ;; Include path should include certain dirs:
                 (let ((P (ede-system-include-path ede-object)))
                   (should (member (expand-file-name "world" testdir) P))
                   (should (member (file-truename builddir) P))
                   )
                 
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
                   )
                 ))

             ;; Try a file in a subdirectory
             (let ((buf (find-file-noselect worldcpp)))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                 ;; Should have set up the current project and target with compilation
                 (should (eq proj (ede-current-project)))
                 (should (oref ede-object compilation))

                 ;; Should have been parsed
                 (should (semantic-active-p))
                 (should (not semantic-parser-warnings))
                 ))

             ;; Try a header file
             (let ((buf (find-file-noselect (expand-file-name "world/world.hpp" testdir))))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                 ;; Should have set up the current project
                 (should ede-object)
                 (should (eq proj (ede-current-project)))
                 ;; Compilation should be pointed to world.cpp
                 (should (eq (oref ede-object compilation)
                             (gethash (file-truename worldcpp) (oref proj compdb))))
                 ))
             
             ;; Try a generated source file
             (let ((buf (find-file-noselect (expand-file-name "build_type.cpp" builddir))))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                 ;; FIXME: Should have set up the current project and target with compilation
                 ;; (should (eq proj (ede-current-project)))
                 ;; (should (oref ede-object compilation))
           
                 ;; FIXME: should have been parsed
                 ;; (should (semantic-active-p))
                 ;; (should (not semantic-parser-warnings))
                 ))

             ;; Force a rescan with all these buffers open, just to make sure it works
             (with-current-buffer (car testbufs)
               (ede-rescan-toplevel))

             ;; All compilation entries should have been updated
             ;; FIXME: need ede-object for all buffers, including above
             (with-current-buffer (get-file-buffer hellocpp)
               ;; Check that compilation entry has been updated
               (should (eq (oref ede-object compilation)
                           (gethash (file-truename buffer-file-name) (oref proj compdb)))))

             ;; Close all buffers and check we can still rescan
             (while testbufs
               (kill-buffer (pop testbufs)))
             (project-rescan proj)

             )
         (while testbufs
           (kill-buffer (pop testbufs)))
         )))))

(ert-deftest compiler-include-path-cache ()
  "Tests that the compiler include paths are detected."
  (cmake-build-directory-fixture t
   (lambda (testdir builddir)
     (let ((savedcache ede-compdb-compiler-cache)
           (proj nil))
       (unwind-protect
           (progn
             (setq ede-compdb-compiler-cache nil)
             (setq proj (ede-add-project-to-global-list 
                         (ede-compdb-project "TESTPROJ"
                                             :compdb-file (expand-file-name "compile_commands.json" builddir)
                                             :file (expand-file-name "CMakeLists.txt" testdir))))
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
           (reldir (file-name-as-directory (concat builddir "release")))
           (custdir (file-name-as-directory (concat builddir "custom"))))
       (make-directory dbgdir)
       (make-directory reldir)
       (make-directory custdir)

       (with-temp-buffer
         (invoke-cmake t ede-compdb-test-srcdir dbgdir "-DCMAKE_BUILD_TYPE=Debug")
         (invoke-cmake t ede-compdb-test-srcdir reldir "-DCMAKE_BUILD_TYPE=Release")
         (invoke-cmake t ede-compdb-test-srcdir custdir "-DCMAKE_BUILD_TYPE=Release")

         (let ((proj (ede-compdb-project
                      "TESTPROJ"
                      :compdb-file "compile_commands.json"
                      :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir)
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
         
           ;; Set the current configuration directory to custdir
           (ede-compdb-set-configuration-directory custdir proj)
           (project-compile-project proj)
           (sleep-until-compilation-done)
           (should (file-executable-p (concat custdir "hello")))

           ))))))

(ert-deftest autoload-project ()
  "Tests that we can autoload a project depending on the presence of a compilation database file"
  (insource-build-fixture
   t
   (lambda (dir)
     (dolist (f '("hello.cpp" "world/world.cpp"))
       (let* ((hellocpp (expand-file-name f dir))
              (buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (ede-current-project))
               (should ede-object)
               (should (oref ede-object compilation)))
           (kill-buffer buf)))))
   ))

(ert-deftest flymake-init-test ()
  "Tests that `ede-compdb-flymake-init' works correctly."
  (cmake-build-directory-fixture
   t
   (lambda (testdir builddir)
     (let* ((proj (ede-add-project-to-global-list
                   (ede-compdb-project "TESTPROJ"
                                       :compdb-file (expand-file-name "compile_commands.json" builddir)
                                       :file (expand-file-name "CMakeLists.txt" testdir))))
            (hellocpp (expand-file-name "hello.cpp" testdir)))

       (let ((buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (eq proj (ede-current-project)))
               (should (oref ede-object compilation))

               (let* ((ret (ede-compdb-flymake-init)))
                 ;; Init function needs to return a list of (compiler, args, dir)
                 (should (listp ret))
                 (should (= 3 (length ret)))

                 ;; Args should be processed correctly
                 (should (cl-search '("-o" "/dev/null") (nth 1 ret) :test 'equal))
                 (should (not (cl-find "-MT" (nth 1 ret) :test 'equal)))
                 (should (not (cl-find hellocpp (nth 1 ret) :test 'equal)))
                 (should (not (cl-find "-c" (nth 1 ret) :test 'equal)))

                 ;; Directory should be the build dir
                 (should (file-equal-p builddir (nth 2 ret)))
                 ))
           (kill-buffer buf)
           ))
       ))))

(ert-deftest ninja-autoload-project ()
  "Tests autoloading of ninja projects when rules.ninja files are discovered"
  ;;:expected-result :passed ;; TODO failed if we can't locate ninja on the path
  (insource-build-fixture
   nil
   (lambda (dir)
     (dolist (f '("hello.cpp" "world/world.cpp"))
       (let* ((hellocpp (expand-file-name f dir))
              (buf (find-file-noselect hellocpp)))
         (unwind-protect
             (with-current-buffer buf
               ;; Should have set up the current project and target
               (should (ede-current-project))
               (should ede-object)
               (should (oref ede-object compilation)))
           (kill-buffer buf))))
     )
   "-G" "Ninja"))
     

(ert-deftest ninja-phony-targets ()
  "Tests that when we are using the ede-ninja-project type, the targets list is populated with phony targets"
  ;;:expected-result :passed ;; TODO failed if we can't locate ninja on the path
  (cmake-build-directory-fixture
   t
   (lambda (testdir builddir)
     (let* ((proj (ede-ninja-project "TESTPROJ"
                                      :compdb-file (expand-file-name "build.ninja" builddir)
                                      :file (expand-file-name "CMakeLists.txt" testdir)
                                      :build-command "ninja")))
       
       (project-compile-project proj)
       (sleep-until-compilation-done)
       (should (file-executable-p (expand-file-name "hello" builddir)))
       
       (project-compile-target proj "clean")
       (sleep-until-compilation-done)
       (should (not (file-executable-p (expand-file-name "hello" builddir))))
       ))
   "-G" "Ninja"))

;;; ede-compdb-test.el ends here
