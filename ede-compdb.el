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
   ;(file :type string :initarg :file)
   (compdb-filename
    :initarg :compdb-filename :type string :initform "compile_commands.json"
    :documentation "The filename for the compilation database, eg \"compile_commmands.json\"")
   (configuration-directories
    :type list :initarg :configuration-directories
    :documentation "For each configuration, a directory in which to locate the configuration database file")
   (build-command
    :type string :initarg :build-command :initform "make -k"
    :documentation "A shell command to build the entire project. Invoked from the configuration directory.")

   (compdb
    :initform (make-hash-table :test 'equal) 
    :documentation "The compilation database, as a hash keyed on source file")

   (file-timestamp
    :initform nil :protection :protected
    :documentation "The last mod time for the compdb file")
   (file-size
    :initform nil :protection :protected
    :documentation "The last measured size of the compdb file")
   )
  )

(defclass ede-compdb-target (ede-target)
  (
   (compilation :type compdb-entry :initarg :compilation)
   (project :type ede-compdb-project :initarg :project)
   )
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

(defmethod initialize-instance :AFTER ((this compdb-entry) &rest fields)

  ;; parse the command line if needed - compiler slot is used to determine whether we need to
  (unless (slot-boundp this :compiler)
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
           ((member argi '("-I" "-F")) (object-add-to-list this :include-path (or argval (pop args)) t))
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
)

(defmethod ede-system-include-path ((this ede-compdb-target))
  "Get the system include path used by project THIS."
  (project-rescan-if-needed (oref this project))
  (mapcar
   (lambda (I)
     (expand-file-name I (file-name-directory (buffer-file-name))))
   (oref (oref this compilation) include-path)))

(defmethod ede-preprocessor-map ((this ede-compdb-target))
  "Get the preprocessor map for target THIS."

  (project-rescan-if-needed (oref this project))

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
    spp))

(defmethod project-compile-target ((this ede-compdb-target) &optional command)
  "Compile the current target THIS.
Argument COMMAND is the command to use for compiling the target."
  (project-rescan-if-needed (oref this project))
  (let* ((entry (oref this compilation))
         (cmd (or command (oref entry command-line)))
         (dir (oref entry directory)))
    ;; TODO: is there a cleaner way to set the build directory?
    (compilation-start (format "cd %s; %s" dir cmd))
    ))



(defmethod current-configuration-directory ((this ede-compdb-project) &optional config)
  "Returns the directory for CONFIG, or the current :configuration-default if not set"
  (let* ((config (or config (oref this configuration-default)))
         (dir (cdr (assoc config
                          (mapcar* #'cons (oref this configurations) (oref this configuration-directories))))))
    (unless dir
      (error "No directory for configuration %s" config))
    (unless (and (file-exists-p dir) (file-directory-p dir))
      (error "Directory not found for configuration %s: %s" config dir))
    dir))
    
(defmethod update-file-from-configuration-default ((this ede-compdb-project))
  "Sets the project's :file from the current :configuration-default. Call this before using the :file slot"
  (let ((configdir (current-configuration-directory this))
        (filedir (file-name-directory (oref this file))))
    (unless (and filedir (file-equal-p configdir filedir))
      (oset this file (concat (file-name-as-directory configdir) (oref this compdb-filename))))
    ))

(defmethod project-rescan ((this ede-compdb-project))
  "Reload the compilation database."
  (let* ((oldprojdir (when (slot-boundp this :directory) (oref this directory)))
         (newprojdir oldprojdir))
    (clrhash (oref this compdb))
    (mapcar (lambda (entry)
              (let* ((directory (file-name-as-directory (cdr (assoc 'directory entry))))
                     (filename (expand-file-name (cdr (assoc 'file entry)) directory))
                     (command-line (cdr (assoc 'command entry)))
                     (target (when (slot-boundp this :targets) (object-assoc filename :path (oref this targets))))
                     (compilation
                      (compdb-entry filename
                                    :command-line command-line
                                    :directory directory))
                     (srcdir (file-name-as-directory (file-name-directory filename))))
                ;; Add this entry to the database
                (puthash filename compilation (oref this compdb))
                ;; Update target if there is one
                (when target
                  (oset this :compilation compilation))
                ;; If we haven't set a project dir, or this entry's directory is a prefix of the
                ;; current project dir, then update the project dir
                (when (or (not newprojdir) (string-prefix-p srcdir newprojdir))
                  (setq newprojdir srcdir))
                ))
            (json-read-file (oref this file)))
            
    (let ((stats (file-attributes (oref this file))))
      (oset this file-timestamp (nth 5 stats))
      (oset this file-size (nth 7 stats)))

    ;; Project may have moved to a new directory - reset if so
    (unless (equal oldprojdir newprojdir)
      (oset this :directory newprojdir)
      (when oldprojdir
        ;; TODO: is this all that is required?
        (ede-project-directory-remove-hash oldprojdir)))
    )
  )

(defmethod project-rescan-if-needed ((this ede-compdb-project))
  "Reload the compilation database if the corresponding watch file has changed."
  (update-file-from-configuration-default this)

  (let ((stats (file-attributes (oref this file))))
    ;; Logic stolen from ede/arduino.el
    (when (or (not (oref this file-timestamp))
              (/= (or (oref this file-size) 0) (nth 7 stats))
              (not (equal (oref this file-timestamp) (nth 5 stats))))
      (project-rescan this))))

(defmethod initialize-instance :AFTER ((this ede-compdb-project) &rest fields)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))  

  (unless (slot-boundp this 'file)
    (oset this file (concat (current-configuration-directory this) (oref this compdb-filename))))

  (unless (slot-boundp this 'compdb-filename)
    (oset this compdb-filename (file-name-nondirectory (oref this file))))

  (unless (slot-boundp this 'configuration-directories)
    (oset this configuration-directories
          (mapcar (lambda (c)
                    ;; Use directory of :file for default configuration, nil for everything else
                    (if (string= c (oref this configuration-default))
                        (file-name-directory (oref this file))
                      nil))
                  (oref this configurations))))
  
  (let ((nconfigs (length (oref this configurations)))
        (ndirs (length (oref this configuration-directories))))
    (unless (= nconfigs ndirs)
      (error "Need %d items in configuration-directories, %d found" nconfigs ndirs)))

  (project-rescan-if-needed this)
  )

(defmethod ede-find-target ((this ede-compdb-project) buffer)
  "Find an EDE target in THIS for BUFFER.
If one doesn't exist, create a new one."
  (let* ((targets (oref this targets))
         (file (expand-file-name (buffer-file-name buffer)))
         (ans (object-assoc file :path targets))
         )
    (when (not ans)
      (setq ans (ede-compdb-target file :compilation (gethash file (oref this compdb)) :project this))
      (object-add-to-list this :targets ans)
      )
    ans))

(defmethod project-compile-project ((this ede-compdb-project))
  "Build the current project using :build-command"
  (let ((default-directory (current-configuration-directory this)))
    (compile (oref this build-command))
    ))

(provide 'ede-compdb)
