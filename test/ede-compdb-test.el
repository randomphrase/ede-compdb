;; ede-compdb-test.el --- ede-compdb unit tests  -*- lexical-binding: t; -*-

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

;; See the documentation at https://github.com/randomphrase/ede-compdb for
;; quickstart and usage information

;;; Code:

;;(require 'ede-compdb)
(require 'ert)
(require 'el-mock)
(require 'semantic)
(require 'cl) ;; needed by el-mock

(ert-deftest parse-command-line ()
  "Tests parsing of command lines"
  (let* ((cmdline "g++ -Dfoo -Dbar=baz -Uqux -isystem /opt/quxx/include -I/opt/local/include -Iincludes -include bar.hpp -imacros config.h -isystem/opt/foo/include -sysroot /sysroot main.cpp")
         (e (compdb-entry "foo.cpp" :directory "." :command-line cmdline))
         ;; expected include dirs
         (incdirs `("/opt/quxx/include" "/opt/local/include" ,(expand-file-name "includes") "/opt/foo/include" ".")))

    (mocklet
     (((ede-compdb-compiler-include-path "g++" ".") => '("/opt/g++/include")))
    
     (should (equal cmdline (get-command-line e)))

     (should (equal '(("foo") ("bar" . "baz")) (oref e defines)))
     (should (equal '("qux") (oref e undefines)))

     (should (equal '("foo" "bar=baz") (get-defines e)))
     (should (equal (append incdirs (list "/opt/g++/include")) (get-include-path e)))
     (should (equal incdirs (get-include-path e t)))
     (should (equal (mapcar #'expand-file-name '("config.h" "bar.hpp")) (get-includes e)))
     )))

(ert-deftest empty-build-dir ()
  "Tests that we can still open files when the build directory can't be located, or is empty"
  (with-temp-directory
   tmpdir
   (let* ((srcdir ede-compdb-test-srcdir)
          (proj (ede-add-project-to-global-list
                 (ede-compdb-project "TESTPROJ"
                                     :compdb-file (expand-file-name "compile_commands.json" tmpdir)
                                     :file (expand-file-name "CMakeLists.txt" srcdir))))
          (maincpp (expand-file-name "main.cpp" srcdir)))
     
     (should (eq proj (ede-directory-get-open-project srcdir)))
     
     (let ((buf (find-file-noselect maincpp)))
       (unwind-protect
           (with-current-buffer buf
             ;; Should have set up the current project and target
             (should (eq proj (ede-current-project)))
             (should (not (oref ede-object compilation)))
             
             ;; Should have been parsed
             (should (semantic-active-p))
             )
         (kill-buffer buf)))
     )))

(ert-deftest parse-clang-system-includes ()
  "Tests discovery of clang system includes"
  (with-temp-buffer
    ;; This is a real example from Apple Clang 5.0
    (insert "clang -cc1 version 5.0 based upon LLVM 3.3svn default target x86_64-apple-darwin13.1.0
ignoring nonexistent directory \"/usr/include/c++/v1\"
ignoring nonexistent directory \"/usr/local/include\"
#include \"...\" search starts here:
#include <...> search starts here:
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/c++/v1
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /usr/include
 /System/Library/Frameworks (framework directory)
 /Library/Frameworks (framework directory)
End of search list.
")
    (goto-char (point-min))
    
    (should (equal (ede-compdb-parse-clang-system-includes)
                   '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/c++/v1"
                     "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/5.0/include"
                     "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                     "/usr/include")))
    ))

(ert-deftest open-file-parsed ()
  "Tests the parsing of source files in a project. We ensure it correctly locates all include files, amongst other things."
  ;;:expected-result :passed ;; TODO failed if we can't locate cmake on the path
  (cmake-build-directory-fixture t
   (lambda (testdir builddir)
     (let ((proj (ede-add-project-to-global-list
                  (ede-compdb-project "TESTPROJ"
                                      :compdb-file (expand-file-name "compile_commands.json" builddir)
                                      :file (expand-file-name "CMakeLists.txt" testdir))))
           (maincpp (expand-file-name "main.cpp" testdir))
           (worldcpp (expand-file-name "world/world.cpp" testdir))
           (utilityhpp (expand-file-name "utility/utility.hpp" testdir))
           (testbufs))

       ;; Basic sanity checks on the project itself
       (should (eq proj (ede-directory-get-open-project testdir)))
       (should (gethash (file-truename maincpp) (oref proj compdb)))

       ;; Now we'll open source files and check that they are parsed correctly
       (unwind-protect
           (progn
             (let ((buf (find-file-noselect maincpp)))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                 ;; Should have set up the current project and target
                 (should ede-object)
                 (should (eq proj (ede-current-project)))
                 (should (oref ede-object compilation))
                 
                 ;; Include path should include certain dirs:
                 (let ((P (ede-system-include-path ede-object)))
                   (should (member (expand-file-name "world" testdir) P))
                   (should (member builddir P))
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

             ;; All of the world.[chi]pp files should also be parsed
             (dolist (EXT '(".cpp" ".hpp" ".ipp"))
               (let* ((filepath (expand-file-name (concat "world/world" EXT) testdir))
                      (buf (find-file-noselect filepath)))
                 (setq testbufs (cons buf testbufs))
                 (with-current-buffer buf
                   (should (eq proj (ede-current-project)))
                   (should (semantic-active-p))
                   (should (not semantic-parser-warnings))

                   ;; Compilation should be pointed to world.cpp
                   (should (eq (oref ede-object compilation)
                               (gethash (file-truename worldcpp) (oref proj compdb))))
                 )))

             ;; Try a header with no matching source file
             (let ((buf (find-file-noselect utilityhpp)))
               (setq testbufs (cons buf testbufs))
               (with-current-buffer buf
                   (should (eq proj (ede-current-project)))
                   (should (semantic-active-p))
                   (should (not semantic-parser-warnings))

                   ;; Compilation should be pointed to main.cpp
                   (should (eq (oref ede-object compilation)
                               (gethash (file-truename maincpp) (oref proj compdb))))
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
               (let (hookrun)

                 ;; Add a local hook so it will go away when we close the buffer
                 (add-hook 'ede-compdb-project-rescan-hook (lambda () (setq hookrun t)) nil t)

                 (ede-rescan-toplevel)
                 
                 ;; Check that the rescan hook is run
                 (should hookrun)))

             ;; All compilation entries should have been updated
             ;; FIXME: need ede-object for all buffers, including above
             (with-current-buffer (get-file-buffer maincpp)
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
               (let* ((maincpp (expand-file-name "main.cpp" testdir))
                      (buf (find-file-noselect maincpp)))
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
  (with-temp-directory
   builddir
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

         )))))

(ert-deftest autoload-project ()
  "Tests that we can autoload a project depending on the presence of a compilation database file"
  (insource-build-fixture
   t
   (lambda (dir)
     (dolist (f '("main.cpp" "world/world.cpp"))
       (let* ((maincpp (expand-file-name f dir))
              (buf (find-file-noselect maincpp)))
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
     (mocklet ((flymake-init-create-temp-buffer-copy => "dummyfile.cpp"))
       (let* ((proj (ede-add-project-to-global-list
                     (ede-compdb-project "TESTPROJ"
                                         :compdb-file (expand-file-name "compile_commands.json" builddir)
                                         :file (expand-file-name "CMakeLists.txt" testdir))))
              (maincpp (expand-file-name "main.cpp" testdir)))
         
         (let ((buf (find-file-noselect maincpp)))
           (unwind-protect
               (with-current-buffer buf
                 ;; Should have set up the current project and target
                 (should (eq proj (ede-current-project)))
                 (should (oref ede-object compilation))

                 (let ((ret (ede-compdb-flymake-init)))
                   ;; Init function needs to return a list of (compiler, args, dir)
                   (should (listp ret))
                   (should (= 3 (length ret)))
                   
                   ;; Args should be processed correctly
                   (should (cl-search '("-o" "/dev/null") (nth 1 ret) :test 'equal))
                   (should (not (cl-find "-MT" (nth 1 ret) :test 'equal)))
                   (should (not (cl-find maincpp (nth 1 ret) :test 'equal)))
                   (should (not (cl-find "-c" (nth 1 ret) :test 'equal)))
                   
                   ;; Directory should be the build dir
                   (should (file-equal-p builddir (nth 2 ret)))
                   )))
           (kill-buffer buf)
           ))
       ))))

(ert-deftest ninja-autoload-project ()
  "Tests autoloading of ninja projects when rules.ninja files are discovered"
  ;;:expected-result :passed ;; TODO failed if we can't locate ninja on the path
  (insource-build-fixture
   nil
   (lambda (dir)
     (dolist (f '("main.cpp" "world/world.cpp"))
       (let* ((maincpp (expand-file-name f dir))
              (buf (find-file-noselect maincpp)))
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
