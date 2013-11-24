;; -*- lexical-binding: t; -*-

(require 'ede)
(require 'json)

(defclass ede-compdb-project (ede-project)
  ( )
  )

(defclass compdb-source-file (eieio-named)
  (
   (command-line :type string :initarg :command-line)
   (compiler :type string :initarg :compiler)
   (include-path :type list :initarg :include-path :initform '())
   (includes :type list :initarg :includes :initform '())
   (defines :type list :initarg :defines :initform '())
   (undefines :type list :initarg :undefines :initform '())
   )
  "A source file in the compilation database"
  )

(defmethod initialize-instance :AFTER ((this compdb-source-file) &rest fields)
  ;(call-next-method)

  ;; parse the command line if needed - compiler slot is used to determine whether we need to
  (unless (slot-boundp this 'compiler)
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
           ((equal argi "-D") (object-add-to-list this 'defines (cons (or argval (pop args)) defval) t))
           ((equal argi "-U") (object-add-to-list this 'undefines (or argval (pop args)) t))
           ((member argi '("-I" "-F")) (object-add-to-list this 'include-path (or argval (pop args)) t))
           ((equal argi "-include") (object-add-to-list this 'includes (pop args) t))
           ;; TODO: -nostdinc, -nostdlibinc, -nobuildinic
           ((not seenopt) (oset this compiler (if (slot-boundp this 'compiler) (concat (oref this compiler) " " argi) argi)))
           )
          (when (char-equal ?- (string-to-char argi))
            (setq seenopt t)
            )
          )
        )
      )
    )
  )

(defun ede-compdb-read-compilation-database-file (file)
  "Reads a compilation database file"
  (let ((hash (make-hash-table :test 'equal)))
    (mapcar (lambda (entry)
              (let ((filename (cdr (assoc 'file entry)))
                    (command-line (cdr (assoc 'command entry))))
                (puthash filename (compdb-source-file filename :command-line command-line) hash)))
            (json-read-file file))
    hash
    ))

(provide 'ede-compdb)
