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
            (call-process "cmake" nil t t "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" testdir)
            ;; TODO: fail if cmake fails?
            (funcall body testdir builddir)
            ))
      (delete-directory builddir t))))

(ert-deftest make-compdb-project ()
  "Tests the parsing of a CMake-generated compile_commands.json file to construct an ede-compdb-project"
  ;;:expected-result :passed ;; TODO failed if we can't locate cmake on the path
  (build-directory-fixture
   (lambda (testdir builddir)
     (should (file-exists-p builddir))
     (let ((proj (ede-compdb-project "TESTPROJ" :file (expand-file-name "compile_commands.json" builddir))))
       (should (hash-table-p (oref proj compdb)))
       (should (< 0 (hash-table-count (oref proj compdb))))
       (should (gethash (expand-file-name "hello.cpp" testdir) (oref proj compdb))))
     )))

(ert-deftest parse-command-line ()
  "Tests parsing of command lines"
  (let ((f (compdb-source-file "foo.cpp" :command-line
                               "clang -Dfoo -Dbar=baz -Uqux -I/opt/local/include -Iincludes -include bar.hpp main.cpp")))
    (should (equal "clang" (oref f compiler)))
    (should (equal '(("foo") ("bar" . "baz")) (oref f defines)))
    (should (equal '("qux") (oref f undefines)))
    (should (equal '("/opt/local/include" "includes") (oref f include-path)))
    (should (equal '("bar.hpp") (oref f includes)))
    )
)
