;;; ede-compdb.el --- Support for compilation database projects in EDE

;; Copyright (C) 2013-2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development ninja build cedet ede
;; Package-Requires: ((ede "1.2") (semantic "2.2") (cl-lib "0.4"))

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

;; EDE-compdb is a library that enables the Emacs Development Environment (EDE),
;; to be used with a compilation database, as provided by build tools such as
;; CMake or Ninja. This enables CEDET to be automatically configured for use to
;; support parsing, navigation, completion, and so on. This is especially useful
;; for C and C++ projects which are otherwise quite tricky to configure for use
;; with CEDET and other libraries.
;; 
;; See the documentation at https://github.com/randomphrase/ede-compdb for
;; quickstart and usage information

;;; Code:

(require 'ede)
(require 'json)

(eval-when-compile
  (require 'cl-lib))

(declare-function ff-other-file-name "find-file")
(declare-function semantic-gcc-fields "semantic/bovine/gcc")
(declare-function semantic-gcc-query "semantic/bovine/gcc")

;;; Autoload support

;;;###autoload
(defun ede-compdb-load-project (dir)
  "Create an instance of option `ede-compdb-project' for DIR."
  ;; TODO: Other project types keep their own cache of active projects - do we need to as well?
  (ede-compdb-project (file-name-nondirectory (directory-file-name dir))
                      :compdb-file "compile_commands.json"
                      :directory (file-name-as-directory dir)))

;;;###autoload
(defun ede-ninja-load-project (dir)
  "Create an instance of option `ede-ninja-project' for DIR."
  ;; TODO: Other project types keep their own cache of active projects - do we need to as well?
  (ede-ninja-project (file-name-nondirectory (directory-file-name dir))
                     :compdb-file "build.ninja"
                     :directory (file-name-as-directory dir)))
                 
;;;###autoload
(eval-after-load "ede/auto"
  (ede-add-project-autoload
   (ede-project-autoload "compdb"
                         :file 'ede-compdb
                         :proj-file "compile_commands.json"
                         :load-type 'ede-compdb-load-project
                         :class-sym 'ede-compdb-project)))

;;;###autoload
(eval-after-load "ede/auto"
  (ede-add-project-autoload
   (ede-project-autoload "ninja"
                         :file 'ede-compdb
                         :proj-file "build.ninja"
                         :load-type 'ede-ninja-load-project
                         :class-sym 'ede-ninja-project)))


;;; Classes:

(defclass compdb-entry (eieio-named)
  (
   (command-line
    :type string :initarg :command-line :protection :protected
    :documentation "The full command line to compile a given file")
   (directory
    :type string :initarg :directory
    :documentation "Directory in which to invoke the compile command")
   (compiler
    :type string :initarg :compiler
    :documentation "The compiler portion of the full command line (may be multi-word)")
   (include-path
    :type list :initarg :include-path :initform '()
    :documentation "List of directories to search for include files")
   (includes
    :type list :initarg :includes :initform '()
    :documentation "List of implicitly included files")
   (defines
    :type list :initarg :defines :initform '()
    :documentation "List of predefined macros")
   (undefines
    :type list :initarg :undefines :initform '()
    :documentation "list of undefined macros")
   )
  "An entry in the compilation database"
  )

(defclass ede-compdb-project (ede-project)
  (
   (keybindings :initform (("b" . ede-project-configurations-set)
                           ("B" . ede-compdb-set-configuration-directory)))
   (compdb-file
    :initarg :compdb-file :type string
    :documentation "The filename for the compilation database, eg \"compile_commmands.json\". This is evaluated relative to the current configuration directory.")
   (configuration-directories
    :type (or list string) :initarg :configuration-directories
    :documentation "For each configuration, a directory in which to locate the configuration database file. This is evaluated relative to :directory")
   (build-command
    :type string :initarg :build-command :initform "make -k"
    :documentation "A shell command to build the entire project. Invoked from the configuration directory.")

   (compdb
    :initform (make-hash-table :test 'equal)
    :documentation "The compilation database, as a hash keyed on source file")

   (compdb-file-timestamp
    :initform nil :protection :protected
    :documentation "The last mod time for the compdb file")
   (compdb-file-size
    :initform nil :protection :protected
    :documentation "The last measured size of the compdb file")
   )
  )

(defclass ede-ninja-project (ede-compdb-project)
  (
   (build-command
    :type string :initarg :build-command :initform "ninja"
    :documentation "A shell command to build the entire project. Invoked from the configuration directory.")
   (phony-targets
    :type list :initform '()
    :documentation "Phony targets which ninja can build")
   )
  "Variant of ede-compdb-project, extended to take advantage of the ninja build tool."
)

(defclass ede-compdb-target (ede-target)
  ;; TODO: this is not really in keeping with the ede-target. Currently we create targets for each source file.
  (
   (compilation :type (or null compdb-entry) :initarg :compilation)
   (project :type ede-compdb-project :initarg :project)
   )
  "Represents a target, namely something that can be built"
  )

;;; Compiler support:

(defvar ede-compdb-compiler-cache nil "Cached include paths for each compiler detected.")

(defun ede-compdb-compiler-include-path (comp)
  "Look up include paths for COMP and add to INCLUDE-PATHS."
  (let ((path (cdr (assoc comp ede-compdb-compiler-cache))))
    (unless path
      (require 'semantic/bovine/gcc)
      ;; FIXME: this is pretty simplistic but it will do for now...
      (setq path (cdr (assoc '--with-gxx-include-dir
                              (semantic-gcc-fields (semantic-gcc-query comp "-v")))))
      (add-to-list 'ede-compdb-compiler-cache (cons comp path)))
    path))


;;; compdb-entry methods:

(defmethod get-command-line ((this compdb-entry))
  ;; TODO: Can this be replaced by an :accessor slot option?
  (parse-command-line-if-needed this)
  (oref this command-line))

(defmethod parse-command-line-if-needed ((this compdb-entry))
  "For performance reasons we delay parsing the compdb command
line until needed. Call this before accessing any slots derived
from the command line (which is most of them!)"
  ;; parse the command line if needed - compiler slot is used to determine whether we need to
  (unless (slot-boundp this :compiler)
    (parse-command-line this)))

(defmethod parse-command-line ((this compdb-entry))
  "Parse the :command-line slot of THIS to derive :compiler, :include-path, etc."
  (let ((args (split-string (oref this command-line)))
        (seenopt nil)
        (case-fold-search nil))
    ;; parsing code inspired by `command-line'
    (while args
      (let ((argi (pop args)) argval defval)
        ;; Handle -DFOO, -UFOO, etc arguments
        (when (string-match "\\`\\(-[DUIF]\\)\\([^=]+\\)\\(=\\(.+\\)\\)?" argi)
          (setq argval (match-string 2 argi))
          (setq defval (match-string 4 argi))
          (setq argi (match-string 1 argi)))
        (cond
         ((equal argi "-D") (object-add-to-list this :defines (cons (or argval (pop args)) defval) t))
         ((equal argi "-U") (object-add-to-list this :undefines (or argval (pop args)) t))
         ;; TODO: support gcc notation "=dir" where '=' is the sysroot prefix
         ((member argi '("-I" "-F" "-isystem")) (object-add-to-list this :include-path (or argval (pop args)) t))
         ((equal argi "-include") (object-add-to-list this :includes (pop args) t))
         ;; TODO: -nostdinc, -nostdlibinc, -nobuildinic
         ((not seenopt) (oset this compiler (if (slot-boundp this :compiler) (concat (oref this compiler) " " argi) argi)))
         )
        (when (char-equal ?- (string-to-char argi))
          (setq seenopt t)
          )))
    
    (let ((cpath (ede-compdb-compiler-include-path (oref this compiler))))
      (when cpath
        (object-add-to-list this :include-path cpath t)))
    ))


;;; ede-compdb-target methods:

(defmethod ede-system-include-path ((this ede-compdb-target))
  "Get the system include path used by project THIS."
  (project-rescan-if-needed (oref this project))
  (let ((comp (oref this compilation)))
    (when comp
      (parse-command-line-if-needed comp)
      (append
       (mapcar
        (lambda (I)
          (expand-file-name I (oref comp directory)))
        (oref comp include-path))
       (list (oref comp directory))
       ))
    ))

(defmethod ede-preprocessor-map ((this ede-compdb-target))
  "Get the preprocessor map for target THIS."
  (project-rescan-if-needed (oref this project))
  (when (oref this compilation)
    (parse-command-line-if-needed (oref this compilation))
    ;; Stolen from cpp-root
    (require 'semantic/db)
    (let ((spp (oref (oref this compilation) defines)))
      (mapc
       (lambda (F)
         (let* ((expfile (expand-file-name F))
                (table (when expfile
                         ;; Disable EDE init on preprocessor file load
                         ;; otherwise we recurse, cause errs, etc.
                         (let ((ede-constructing t))
                           (semanticdb-file-table-object expfile))))
                )
           (cond
            ((not (file-exists-p expfile))
             (message "Cannot find file %s in project." F))
            ((string= expfile (buffer-file-name))
             ;; Don't include this file in it's own spp table.
             )
            ((not table)
             (message "No db table available for %s." expfile))
            (t
             (when (semanticdb-needs-refresh-p table)
               (semanticdb-refresh-table table))
             (setq spp (append spp (oref table lexical-table)))))))
       (oref (oref this compilation) includes))
      spp)))

(defmethod project-compile-target ((this ede-compdb-target))
  "Compile the current target THIS."
  (project-compile-target (oref this project) this))

(defun ede-object-system-include-path ()
  "Returns the system include path for the current buffer"
  (when ede-object
    (ede-system-include-path ede-object)))

(defun ede-compdb-flymake-init ()
  "Init function suitable for use with `flymake-mode'."
  (when (and ede-object (slot-boundp ede-object :compilation) (oref ede-object :compilation))
    (let* ((comp (oref ede-object compilation))
           (args (split-string (get-command-line comp)))
           (temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           ret)
      ;; Process args, building up a new list as we go. Each new element is added to the head of the
      ;; list, so we need to reverse it once done
      (while args
        (let ((argi (pop args)))
          (cond
            ;; substitude /dev/null for the output file
           ((equal argi "-o")
            (setq ret (cons "/dev/null" (cons argi ret)))
            (pop args))

            ;; substitute -S for -c (ie just compile, don't assemble)
           ((equal argi "-c")
            (setq ret (cons "-S" ret)))

           ;; Don't do any makefile generation
           ((member argi '("-M" "-MM" "-MMD" "-MG" "-MP" "-MD")))
           ((member argi '("-MF" "-MT" "-MQ"))
            (pop args))

            ;; substitute temp-file for the input file
           ((file-equal-p (expand-file-name argi (oref comp directory)) buffer-file-name)
            (setq ret (cons temp-file ret)))
           (t
            (setq ret (cons argi ret)))
           )))
      (setq ret (reverse ret))
      (list (pop ret) ret (oref comp directory))
      )))


;;; ede-compdb-project methods:

(defmethod current-configuration-directory-path ((this ede-compdb-project) &optional config)
  "Returns the path to the configuration directory for CONFIG, or for :configuration-default if CONFIG not set"
  (let ((dir (nth (cl-position (or config (oref this configuration-default)) (oref this configurations) :test 'equal)
                  (oref this configuration-directories))))
    (and dir (file-name-as-directory (expand-file-name dir (oref this directory))))))

(defmethod current-configuration-directory ((this ede-compdb-project) &optional config)
  "Returns the validated configuration directory for CONFIG, or for :configuration-default if CONFIG not set"
  (let ((dir (current-configuration-directory-path this config)))
    (unless dir
      (error "No directory for configuration %s" config))
    (unless (and (file-exists-p dir) (file-directory-p dir))
      (error "Directory not found for configuration %s: %s" config dir))
    dir))
    
(defmethod set-configuration-directory ((this ede-compdb-project) dir &optional config)
  "Sets the directory for configuration CONFIG to DIR.  The
current configuration directory is used if CONFIG not set."
  (let ((config (or config (oref this configuration-default))))
    (setcar (nthcdr (cl-position config(oref this configurations) :test 'equal)
                    (oref this configuration-directories))
            dir)
    (message "Configuration \"%s\" directory set to: %s" config dir)))

(defmethod current-compdb-path ((this ede-compdb-project))
  "Returns a path to the current compdb file"
  (expand-file-name (oref this compdb-file) (current-configuration-directory-path this)))

(defmethod insert-compdb ((_this ede-compdb-project) compdb-path)
  "Inserts the compilation database into the current buffer"
  (insert-file-contents compdb-path))

(defmethod compdb-entry-for-buffer ((this ede-compdb-project))
  "Returns an instance of ede-compdb-entry suitable for use with
the current buffer. In general, we do a lookup on the current
buffer file in the compdb hashtable. If not present, we do a
lookup on the filename calculated from `ff-other-file-name'."
  (require 'find-file)
  (let ((ret (gethash (file-truename (buffer-file-name)) (oref this compdb)))
        (ignore ff-ignore-include)
        other-name)
    (or ret
        (progn
          (setq ff-ignore-include t)
          (setq other-name (ff-other-file-name))
          (when other-name
            (setq ret (gethash (file-truename other-name) (oref this compdb))))
          (setq ff-ignore-include ignore)
          ret))
    ))

(defmethod project-rescan ((this ede-compdb-project))
  "Reload the compilation database."
  (clrhash (oref this compdb))
  (let* ((compdb-path (current-compdb-path this))
         (builddir (current-configuration-directory this))
         (oldprojdir (oref this directory))
         (newprojdir oldprojdir)
         ;; externbuild set for out-of-source builds
         (externbuild (when (not (string-prefix-p oldprojdir builddir)) builddir))
         (json-array-type 'list)
         json-compdb)

    (with-temp-buffer
      (insert-compdb this compdb-path)
      (goto-char (point-min))
      (message "Reading Compilation Database from %s ..." compdb-path)
      (setq json-compdb (json-read)))

    (let ((progress-reporter (make-progress-reporter "Building Compilation Entries..." 0 (length json-compdb)))
          (iter 1))
      
      (dolist (E json-compdb)
        (let* ((directory (file-name-as-directory (cdr (assoc 'directory E))))
               (filename (expand-file-name (cdr (assoc 'file E)) directory))
               (filetruename (file-truename filename))
               (command-line (cdr (assoc 'command E)))
               (compilation
                (compdb-entry filename
                              :command-line command-line
                              :directory directory))
               (srcdir (file-name-as-directory (file-name-directory filename))))

          ;; Add this entry to the database
          (puthash filetruename compilation (oref this compdb))
          
          ;; If we haven't set a project dir, or this entry's directory is a prefix of the current
          ;; project dir, then update the project dir. However, we ignore external build
          ;; directories, because they could be in a completely different part of the filesystem.
          (when (and (or (not externbuild) (not (string-prefix-p externbuild newprojdir)))
                     (or (not newprojdir) (string-prefix-p srcdir newprojdir)))
            (setq newprojdir srcdir))

          (progress-reporter-update progress-reporter iter)
          (setq iter (1+ iter))
          ))
      
      (progress-reporter-done progress-reporter)
      )
            
    (let ((stats (file-attributes compdb-path)))
      (oset this compdb-file-timestamp (nth 5 stats))
      (oset this compdb-file-size (nth 7 stats)))

    ;; Project may have moved to a new directory - reset if so
    (unless (equal oldprojdir newprojdir)
      (oset this :directory newprojdir)
      (when oldprojdir
        ;; TODO: is this all that is required?
        (ede-project-directory-remove-hash oldprojdir)))

    ;; Remove targets without a buffer - we won't be able to update the compilation entry otherwise
    (oset this targets
          (remq nil
                (mapcar (lambda (T) (when (get-file-buffer (expand-file-name (oref T path) oldprojdir)) T))
                        (oref this targets))))

    ;; Update all remaining targets
    (dolist (T (oref this targets))

      ;; Update compilation
      (with-current-buffer (get-file-buffer (expand-file-name (oref T :path) oldprojdir))
        (oset T :compilation (compdb-entry-for-buffer this)))

      (when (and (not (equal oldprojdir newprojdir)) (slot-boundp T 'path))
        (oset T :path (ede-convert-path this (expand-file-name (oref T path) oldprojdir))))
      )
    ))

(defmethod project-rescan-if-needed ((this ede-compdb-project))
  "Reload the compilation database if the corresponding watch file has changed."
  (let ((stats (file-attributes (current-compdb-path this))))
    ;; Logic stolen from ede/arduino.el
    ;; stats will be null if compdb file is not present
    (when (and stats
               (or (not (oref this compdb-file-timestamp))
                   (/= (or (oref this compdb-file-size) 0) (nth 7 stats))
                   (not (equal (oref this compdb-file-timestamp) (nth 5 stats)))))
      (project-rescan this))))

(defmethod initialize-instance :AFTER ((this ede-compdb-project) &rest _fields)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))

  (unless (slot-boundp this 'compdb-file)
    (oset this compdb-file (file-name-nondirectory (expand-file-name (oref this file)))))

  (unless (slot-boundp this 'configuration-directories)
    ;; Short-cut: set the current configuration directory by supplying a full pathname to :compdb-file
    (let ((confdir (or (file-name-directory (oref this compdb-file)) ".")))
      (oset this configuration-directories
            (mapcar (lambda (c)
                      (if (string= c (oref this configuration-default))
                          confdir nil))
                  (oref this configurations)))))

  (let ((nconfigs (length (oref this configurations)))
        (ndirs (length (oref this configuration-directories))))
    (unless (= nconfigs ndirs)
      (error "Need %d items in configuration-directories, %d found" nconfigs ndirs)))

  ;; Needed if we've used the above short-cut
  (oset this compdb-file (file-name-nondirectory (oref this compdb-file)))

  (unless (slot-boundp this 'file)
    ;; Set the :file from :directory/:compdb-file
    (oset this file (expand-file-name (oref this compdb-file) (oref this directory))))

  (unless (slot-boundp this 'directory)
    ;; set a starting :directory so that we can evaluate current-configuration-directory
    (oset this directory (file-name-directory (expand-file-name (oref this file)))))

  ;; Rescan if compdb exists
  (if (file-exists-p (current-compdb-path this))
      (project-rescan this)
    (message "Error reading Compilation Database: %s not found" (current-compdb-path this)))
  )

(defmethod ede-find-subproject-for-directory ((proj ede-compdb-project)
                                              _dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((this ede-compdb-project) buffer)
  "Find an EDE target in THIS for BUFFER.
If one doesn't exist, create a new one."
  (let* ((path (ede-convert-path this buffer-file-name))
         (ans (object-assoc path :path (oref this targets))))
    (when (not ans)
      (project-rescan-if-needed this)
      (with-current-buffer buffer
        (setq ans (ede-compdb-target
                   path
                   :path path
                   :compilation (compdb-entry-for-buffer this)
                   :project this)))
      (object-add-to-list this :targets ans)
      )
    ans))

(defmethod project-compile-target ((this ede-compdb-project) target)
  "Build TARGET using :build-command. TARGET may be an instance
of `ede-compdb-target' or a string."
  (project-rescan-if-needed this)
  (let* ((entry (when (and (ede-compdb-target-p target) (slot-boundp target :compilation))
                  (oref target compilation)))
         (cmd (if entry (get-command-line entry)
                (concat (oref this build-command) " "
                        (if (ede-compdb-target-p target) (oref target name) target))))
         (default-directory (if entry (oref entry directory)
                              (current-configuration-directory this))))
    (compilation-start cmd)
    ))

(defmethod project-compile-project ((this ede-compdb-project))
  "Build the project THIS using :build-command"
  (let ((default-directory (current-configuration-directory this)))
    (compilation-start (oref this build-command))
    ))

(defmethod ede-menu-items-build ((_this ede-compdb-project) &optional _current)
  "Override to add a custom target menu item"
  (append (call-next-method)
          (list
           [ "Set Configuration Directory..." ede-compdb-set-configuration-directory ])))

(defun ede-compdb-set-configuration-directory (dir &optional proj config)
  "Set the configuration directory of project PROJ for configuration CONFIG to DIR."
  (interactive "DConfiguration Directory: ")
  (set-configuration-directory (or proj (ede-current-project)) dir config))

;;; ede-compdb-project methods:

(defvar ede-ninja-target-regexp "^\\(.+\\): \\(phony\\|CLEAN\\)$"
  "Regexp to identify phony targets in the output of ninja -t targets.")

(defmethod project-rescan ((this ede-ninja-project))
  "Get ninja to describe the set of phony targets, add them to the target list"
  (call-next-method)
  (with-temp-buffer
    (let ((default-directory (current-configuration-directory this)))
      (oset this phony-targets nil)
      (erase-buffer)
      (call-process "ninja" nil t t "-t" "targets")
      (let ((progress-reporter (make-progress-reporter "Scanning targets..." (point-min) (point-max))))
        (goto-char 0)
        (while (re-search-forward ede-ninja-target-regexp nil t)
          ;; Don't use object-add-to-list, it is too slow
          (oset this phony-targets (cons (match-string 1) (oref this phony-targets)))
          (progress-reporter-update progress-reporter (point))
          )
        (progress-reporter-done progress-reporter))
      )))

(defmethod insert-compdb ((_this ede-ninja-project) compdb-path)
  "Use ninja's compdb tool to insert the compilation database
into the current buffer. COMPDB-PATH represents the current path
to :compdb-file"
  (message "Building compilation database...")
  (let ((default-directory (file-name-directory compdb-path)))
    (call-process "ninja" nil t nil "-t" "compdb" "CXX_COMPILER" "C_COMPILER")))

(defmethod project-interactive-select-target ((this ede-ninja-project) prompt)
  "Interactively query for a target. Argument PROMPT is the prompt to use."
  (let ((tname (completing-read prompt (oref this phony-targets) nil nil nil 'ede-ninja-target-history)))
    ;; Create a new target and return it - doesn't matter that it's not in :targets list...
    (ede-compdb-target tname :name tname :project this)))

(provide 'ede-compdb)

;;; ede-compdb.el ends here
