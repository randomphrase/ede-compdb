;; -*- lexical-binding: t; -*-

(require 'ede-compdb)
(require 'ert)

(defun build-directory-fixture (body)
  (let ((builddir (file-name-as-directory (make-temp-file "build-" t))))
    (unwind-protect
        (with-current-buffer (get-buffer-create "*ede-compdb-test*")
          (let ((testdir (expand-file-name "test" (when load-file-name (file-name-directory load-file-name))))
                (default-directory builddir))
            (erase-buffer)
            (let ((ret (call-process "cmake" nil t t "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" testdir)))
              (when (> 0 ret)
                (error "Error running CMake: error %d" ret)))
            (funcall body (file-name-as-directory testdir) builddir)
            ))
      (progn
        (delete-directory builddir t)
        (ede-flush-deleted-projects)))))

(ert-deftest make-compdb-project ()
  "Tests the parsing of a CMake-generated compile_commands.json file to construct an ede-compdb-project"
  ;;:expected-result :passed ;; TODO failed if we can't locate cmake on the path
  (build-directory-fixture
   (lambda (testdir builddir)
     (should (file-exists-p builddir))

     (let ((proj (ede-add-project-to-global-list
                  (ede-compdb-project "TESTPROJ" :directory testdir :file (expand-file-name "compile_commands.json" builddir))))
           (hellocpp (expand-file-name "hello.cpp" testdir)))

       ;; Basic sanity checks on the project itself
       (should (eq proj (ede-directory-get-open-project testdir)))
       (should (gethash hellocpp (oref proj compdb)))

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

               ;; All includes should be parsed
               (dolist (inc (semantic-find-tags-by-class 'include))
                       (should (semantic-dependency-tag-file inc)))
               )
           (kill-buffer buf)))
       ))))

(ert-deftest parse-command-line ()
  "Tests parsing of command lines"
  (let ((f (compdb-entry "foo.cpp" :command-line
                         "clang -Dfoo -Dbar=baz -Uqux -I/opt/local/include -Iincludes -include bar.hpp main.cpp")))
    (should (equal "clang" (oref f compiler)))
    (should (equal '(("foo") ("bar" . "baz")) (oref f defines)))
    (should (equal '("qux") (oref f undefines)))
    (should (equal '("/opt/local/include" "includes") (oref f include-path)))
    (should (equal '("bar.hpp") (oref f includes)))
    )
)
