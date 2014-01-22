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

(defclass ede-compdb-target (ede-target)
  (
   (compilation :type (or null compdb-entry) :initarg :compilation)
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
  (let ((comp (oref this compilation)))
    (when comp
      (mapcar
       (lambda (I)
         (expand-file-name I (file-name-directory (buffer-file-name))))
       (oref comp include-path)))))

(defmethod ede-preprocessor-map ((this ede-compdb-target))
  "Get the preprocessor map for target THIS."

  (project-rescan-if-needed (oref this project))

  (when (oref this compilation)
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
         (dirname (cdr (assoc config
                          (mapcar* #'cons (oref this configurations) (oref this configuration-directories)))))
         (dir (and dirname (expand-file-name dirname (oref this directory)))))
    (unless dir
      (error "No directory for configuration %s" config))
    (unless (and (file-exists-p dir) (file-directory-p dir))
      (error "Directory not found for configuration %s: %s" config dir))
    dir))
    
(defmethod current-compdb-path ((this ede-compdb-project))
  "Returns a full path to the current compdb file"
  (expand-file-name (oref this compdb-file) (current-configuration-directory this)))

(defmethod project-rescan ((this ede-compdb-project))
  "Reload the compilation database."
  (let* ((compdb-path (current-compdb-path this))
         (oldprojdir (when (slot-boundp this :directory) (oref this directory)))
         (newprojdir oldprojdir))
    (clrhash (oref this compdb))
    (mapcar (lambda (entry)
              (let* ((directory (file-name-as-directory (cdr (assoc 'directory entry))))
                     (filename (expand-file-name (cdr (assoc 'file entry)) directory))
                     (filetruename (file-truename filename))
                     (command-line (cdr (assoc 'command entry)))
                     (target (when (slot-boundp this :targets) (object-assoc filename :path (oref this targets))))
                     (compilation
                      (compdb-entry filename
                                    :command-line command-line
                                    :directory directory))
                     (srcdir (file-name-as-directory (file-name-directory filename))))
                ;; Add this entry to the database
                (puthash filetruename compilation (oref this compdb))
                ;; Update target if there is one
                (when target
                  (oset this :compilation compilation))
                ;; If we haven't set a project dir, or this entry's directory is a prefix of the
                ;; current project dir, then update the project dir
                (when (or (not newprojdir) (string-prefix-p srcdir newprojdir))
                  (setq newprojdir srcdir))
                ))
            (json-read-file compdb-path))
            
    (let ((stats (file-attributes compdb-path)))
      (oset this compdb-file-timestamp (nth 5 stats))
      (oset this compdb-file-size (nth 7 stats)))

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
  (let ((stats (file-attributes (current-compdb-path this))))
    ;; Logic stolen from ede/arduino.el
    (when (or (not (oref this compdb-file-timestamp))
              (/= (or (oref this compdb-file-size) 0) (nth 7 stats))
              (not (equal (oref this compdb-file-timestamp) (nth 5 stats))))
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
    ;; set a starting :directory so that we can evaluate current-configuration-directory, needed for project-rescan
    (oset this directory (file-name-directory (expand-file-name (oref this file)))))

  (project-rescan this)
  )

(defmethod ede-find-subproject-for-directory ((proj ede-compdb-project)
                                              dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((this ede-compdb-project) buffer)
  "Find an EDE target in THIS for BUFFER.
If one doesn't exist, create a new one."
  (let* ((targets (oref this targets))
         (file (file-truename (buffer-file-name buffer)))
         (ans (object-assoc file :path targets)))
    (when (not ans)
      (project-rescan-if-needed this)
      (setq ans (ede-compdb-target file :compilation (gethash file (oref this compdb)) :project this))
      (object-add-to-list this :targets ans)
      )
    ans))

(defmethod project-compile-project ((this ede-compdb-project))
  "Build the current project using :build-command"
  (let ((default-directory (current-configuration-directory this)))
    (compilation-start (format "cd %s; %s" default-directory (oref this build-command)))
    ))

(defun compdb-load-project (dir)
  "Creates an ede-compdb project for DIR"
  ;; TODO: Other project types keep their own cache of active projects - do we need to as well?
  (ede-add-project-to-global-list
   (ede-compdb-project (file-name-nondirectory (directory-file-name dir))
                       :compdb-file "compile_commands.json"
                       :directory (file-name-as-directory dir))))
                 
;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "compdb"
                       :file 'ede-compdb
                       :proj-file "compile_commands.json"
                       :load-type 'compdb-load-project
                       :class-sym 'ede-compdb-project)
 'unique)

(provide 'ede-compdb)
