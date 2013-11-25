;; -*- lexical-binding: t; -*-

(require 'ede)
(require 'json)

(defclass ede-compdb-project (ede-project)
  (
   (file :type string :initarg :file)
   (compdb :initform (make-hash-table :test 'equal) 
           :documentation "The compilation database, as a hash keyed on source file")
   (file-timestamp
    :initform nil
    :protection :protected
    :documentation "The last mod time for the compdb file")
   (file-size
    :initform nil
    :protection :protected
    :documentation "The last measured size of the compdb file")
   )
  )

(defclass compdb-entry (eieio-named)
  (
   (command-line :type string :initarg :command-line)
   (compiler :type string :initarg :compiler)
   (include-path :type list :initarg :include-path :initform '())
   (includes :type list :initarg :includes :initform '())
   (defines :type list :initarg :defines :initform '())
   (undefines :type list :initarg :undefines :initform '())
   )
  "An entry in the compilation database"
  )

(defclass ede-compdb-target (ede-target)
  (
   (compilation :type compdb-entry :initarg :compilation)
   )
)

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
      )))

(defmethod project-rescan ((this ede-compdb-project))
  "Reload the compilation database."
  (clrhash (oref this compdb))
  (mapcar (lambda (entry)
            (let* ((filename (cdr (assoc 'file entry)))
                   (command-line (cdr (assoc 'command entry)))
                   (target (if (slot-boundp this :targets) (object-assoc filename :path (oref this targets)) nil))
                   (compilation (compdb-entry filename :command-line command-line)))
              (puthash filename compilation (oref this compdb))
              (when target
                (oset this :compilation compilation))))
          (json-read-file (oref this file)))

  (let ((stats (file-attributes (oref this file))))
    (oset this file-timestamp (nth 5 stats))
    (oset this file-size (nth 7 stats))))

(defmethod project-rescan-if-needed ((this ede-compdb-project))
  "Reload the compilation database if the corresponding watch file has changed."
  (let ((stats (file-attributes (oref this file))))
    ;; Logic stolen from ede/arduino.el
    (when (or (not (oref this file-timestamp))
              (/= (or (oref this file-size) 0) (nth 7 stats))
              (not (equal (oref this file-timestamp) (nth 5 stats))))
      (project-rescan this))))

(defmethod initialize-instance :AFTER ((this ede-compdb-project) &rest fields)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))  

  (project-rescan-if-needed this)
  )

(defmethod ede-system-include-path ((this ede-compdb-target))
  "Get the system include path used by project THIS."
  (oref (oref this compilation) include-path))

(defmethod ede-find-target ((this ede-compdb-project) buffer)
  "Find an EDE target in THIS for BUFFER.
If one doesn't exist, create a new one."
  (let* ((targets (oref this targets))
         (file (expand-file-name (buffer-file-name buffer)))
         (ans (object-assoc file :path targets))
         )
    (when (not ans)
      (setq ans (ede-compdb-target file :compilation (gethash file (oref this compdb))))
      (object-add-to-list this :targets ans)
      )
    ans))


(provide 'ede-compdb)
