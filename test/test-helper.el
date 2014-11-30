;; test-helper.el --- Helpers for ede-compdb unit tests  -*- lexical-binding: t; -*-

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

(require 'ede)
(require 'semantic)
(require 'f)
(require 'cl-lib)
(require 'undercover)

(defvar ede-compdb-test-path (f-dirname (f-this-file)))
(defvar ede-compdb-path (f-parent ede-compdb-test-path))

(undercover "ede-compdb.el")

(require 'ede-compdb (f-expand "ede-compdb.el" ede-compdb-path))

;; Workaround needed because ede-flush-deleted-projects is not in Emacs 24.3
(unless (fboundp 'ede-flush-deleted-projects)
  (defun ede-flush-deleted-projects ()
    (cl-delete-if (lambda (P) (not (file-exists-p (oref P :file))))
                  ede-projects)
    ))

;; Need to have EDE and semantic automatically enabled for new buffers
(semantic-mode t)
(global-ede-mode t)

;; Inhibit the "Create directory for SemanticDB?" question
;; TOOD: Is this a bug in semantic?
(when noninteractive
  (remove-hook 'kill-emacs-hook 'semanticdb-kill-emacs-hook))

;; We test with .ipp files - map to c++ mode so they can be parsed correctly
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))

(defvar ede-compdb-test-srcdir
  (file-name-as-directory 
   (expand-file-name "proj" (when load-file-name (file-name-directory load-file-name)))))

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.  The temporary directory will be deleted on exit/error."
  `(let ((,dir (file-name-as-directory (make-temp-file "build-" t))))
    (unwind-protect
        ,@body
      (progn
        (delete-directory ,dir t)
        (ede-flush-deleted-projects)))))

(defun invoke-cmake (gencompdb srcdir builddir &rest args)
  "Invoke cmake on the SRCDIR to build into BUILDDIR with ARGS.
If GENCOMPDB is non-nill, a compilation database will be
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
  (with-temp-directory
   builddir
   (should (file-exists-p builddir))
   (with-current-buffer (get-buffer-create "*ede-compdb-test*")
     (apply 'invoke-cmake (append (list gencompdb ede-compdb-test-srcdir builddir) args))
     (funcall body ede-compdb-test-srcdir builddir)
     )))

(defun sleep-until-compilation-done ()
  (let* ((comp-buf (get-buffer "*compilation*"))
         (comp-proc (get-buffer-process comp-buf)))
    (while comp-proc
      (sleep-for 1)
      (setq comp-proc (get-buffer-process comp-buf)))))

(defun insource-build-fixture (gencompdb body &rest args)
  "Sets up a source tree in a temporary directory for an
in-source build"
  (with-temp-directory
   builddir
   ;; To keep the source directory clean, we'll copy the test project into the temp directory
   (copy-directory ede-compdb-test-srcdir builddir)
   (let ((srcdir (file-name-as-directory (concat builddir "proj"))))
     (apply 'invoke-cmake (append (list gencompdb "." srcdir) args))
     (funcall body srcdir)
     )))

;;; test-helper.el ends here
