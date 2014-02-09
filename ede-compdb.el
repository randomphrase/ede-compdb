;; -*- lexical-binding: t; -*-

(require 'ede)
(require 'json)

(defclass compdb-entry (eieio-named)
  (
   (command-line
    :type string :initarg :command-line
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

(defvar ede-compdb-compiler-cache nil "Cached include paths for each compiler detected")

(defun ede-compdb-compiler-include-path (comp)
  "Looks up include paths for COMP and adds to INCLUDE-PATHS"
  (let ((path (cdr (assoc comp ede-compdb-compiler-cache))))
    (unless path
      (require 'semantic/bovine/gcc)
      ;; FIXME: this is pretty simplistic but it will do for now...
      (setq path (cdr (assoc '--with-gxx-include-dir
                              (semantic-gcc-fields (semantic-gcc-query comp "-v")))))
      (add-to-list 'ede-compdb-compiler-cache (cons comp path)))
    path))

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
  ;; parsing code inspired by `command-line'
  (let ((args (split-string (oref this command-line)))
        (seenopt nil)
        (case-fold-search nil))
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

(defmethod project-compile-target ((this ede-compdb-target) &optional command)
  "Compile the current target THIS.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-target (oref this project) this))



(defmethod current-configuration-directory-path ((this ede-compdb-project) &optional config)
  "Returns the path to the configuration directory for CONFIG, or for :configuration-default if CONFIG not set"
  (let ((dir (cdr (assoc (or config (oref this configuration-default))
                         (mapcar* #'cons (oref this configurations) (oref this configuration-directories))))))
    (and dir (file-name-as-directory (expand-file-name dir (oref this directory))))))

(defmethod current-configuration-directory ((this ede-compdb-project) &optional config)
  "Returns the validated configuration directory for CONFIG, or for :configuration-default if CONFIG not set"
  (let ((dir (current-configuration-directory-path this config)))
    (unless dir
      (error "No directory for configuration %s" config))
    (unless (and (file-exists-p dir) (file-directory-p dir))
      (error "Directory not found for configuration %s: %s" config dir))
    dir))
    
(defmethod current-compdb-path ((this ede-compdb-project))
  "Returns a path to the current compdb file"
  (expand-file-name (oref this compdb-file) (current-configuration-directory-path this)))

(defmethod insert-compdb ((this ede-compdb-project) compdb-path)
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
      (message "Reading Compilation Database...")
      (setq json-compdb (json-read)))

    (let ((progress-reporter (make-progress-reporter (format "Building Compilation Entries..." compdb-path) 0 (length json-compdb)))
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
          (cl-incf iter)
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

    ;; Update all remaining targets
    (dolist (T (oref this targets))

      ;; Update compilation
      (when (slot-boundp T :compilation)
        (with-current-buffer (get-file-buffer (expand-file-name (oref T :path) oldprojdir))
          (oset T :compilation (compdb-entry-for-buffer this))))

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

(defmethod initialize-instance :AFTER ((this ede-compdb-project) &rest fields)
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
                                              dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((this ede-compdb-project) buffer)
  "Find an EDE target in THIS for BUFFER.
If one doesn't exist, create a new one."
  (let* ((file (file-truename (buffer-file-name buffer))) ; FIXME: not right - use ede-convert-path
         (ans (object-assoc file :path (oref this targets))))
    (when (not ans)
      (project-rescan-if-needed this)
      (with-current-buffer buffer
        (setq ans (ede-compdb-target
                   (ede-convert-path this file)
                   :path (ede-convert-path this file)
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
  "Build the current project using :build-command"
  (let ((default-directory (current-configuration-directory this)))
    (compilation-start (oref this build-command))
    ))

(defmethod ede-menu-items-build ((this ede-compdb-project) &optional current)
  "Override to add a custom target menu item"
  (append (call-next-method)
          (list
           [ "Build Other Target..." ede-compdb-build-target ])))

(defun ede-compdb-build-target (target)
  "Prompt for a custom target and build it in the current project"
  (interactive
   (let* ((proj (ede-current-project))
          (targets (oref proj phony-targets))
          (string (completing-read "Target: " targets nil nil nil 'ede-compdb-target-history)))
     (list string)))
  (let ((proj (ede-current-project)))
    (project-compile-target proj target)))



(defvar ede-ninja-target-regexp "^\\(.+\\): \\(phony\\|CLEAN\\)$")

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

(defmethod insert-compdb ((this ede-ninja-project) compdb-path)
  "Use ninja's compdb tool to insert the compilation database into the current buffer"
  (message "Building compilation database...")
  (let ((default-directory (file-name-directory compdb-path)))
    (call-process "ninja" nil t nil "-t" "compdb" "CXX_COMPILER" "C_COMPILER")))

;;; Autoload support

(defun ede-compdb-load-project (dir)
  "Creates an ede-compdb project for DIR"
  ;; TODO: Other project types keep their own cache of active projects - do we need to as well?
  (ede-compdb-project (file-name-nondirectory (directory-file-name dir))
                      :compdb-file "compile_commands.json"
                      :directory (file-name-as-directory dir)))

(defun ede-ninja-load-project (dir)
  "Creates an ede-ninja project for DIR"
  ;; TODO: Other project types keep their own cache of active projects - do we need to as well?
  (ede-ninja-project (file-name-nondirectory (directory-file-name dir))
                     :compdb-file "build.ninja"
                     :directory (file-name-as-directory dir)))
                 
;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "compdb"
                       :file 'ede-compdb
                       :proj-file "compile_commands.json"
                       :load-type 'ede-compdb-load-project
                       :class-sym 'ede-compdb-project))
 ;;'unique)

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "ninja"
                       :file 'ede-compdb
                       :proj-file "build.ninja"
                       :load-type 'ede-ninja-load-project
                       :class-sym 'ede-ninja-project))
 ;;'unique)

(provide 'ede-compdb)
