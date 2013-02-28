;; nailgun.el

(require 'bindat)

(defun nailgun-reset (nailgun)
  (setf nailgun-chunk-read 0
        nailgun-chunk-length nil
        nailgun-chunk-type nil)
    (with-current-buffer (process-buffer nailgun)
      (set-buffer-multibyte nil)
      (erase-buffer)))

(defun nailgun-connect (host port callback)
  (let ((nailgun
         (make-network-process
          :name "*nailgun*"
          :buffer "*nailgun*"
          :host host
          :service port
          :coding 'no-conversion
          :filter-multi-byte nil
          :filter (lambda (nailgun string)
                    (nailgun-process-filter nailgun string callback)))))
    (nailgun-reset nailgun)
    nailgun))

(defun nailgun-disconnect (nailgun)
  (unless (eql nailgun-chunk-read 0)
    (warn "unread nailgun chunk data remaining"))
  (delete-process nailgun))

(defconst nailgun-chunk-length-spec
  '((:length u32)))
(defconst nailgun-chunk-length-length 4)

(defconst nailgun-chunk-header-spec
  `(,@nailgun-chunk-length-spec
    (:type u8)))
(defconst nailgun-chunk-header-length 5)

(defconst nailgun-chunk-spec
  `(,@nailgun-chunk-header-spec
    (:payload vec (:length))))

(defvar nailgun-types
  '((:argument          . ?A)
    (:environment       . ?E)
    (:working-directory . ?D)
    (:command           . ?C)
    (:stdin             . ?0)
    (:stdout            . ?1)
    (:stderr            . ?2)
    (:start-reading     . ?S)
    (:stdin-eof         . ?.)
    (:exit              . ?X)
    ;; is this one documented somewhere?
    (:keep-alive        . ?K)))

(defun nailgun-keyword-to-type (keyword)
  (or (cdr (assoc keyword nailgun-types))
      (error "unknown nailgun keyword %s" keyword)))

(defun nailgun-type-to-keyword (type)
  (or (car (rassoc type nailgun-types))
      (error "unknown nailgun type %c" type)))

(defmacro nailgun-type-case (expr &rest clauses)
  "Like CASE, but only uses keywords from NAILGUN-TYPES as key, or T/NIL
as a single clause.
\n(fn EXPR (KEYLIST BODY...)...)"
  `(case ,expr
     ,@(mapcar (lambda (clause)
                 (let ((car (car clause)))
                   (if (or (eq car t) (eq car nil))
                       clause
                     `(,(mapcar 'nailgun-keyword-to-type
                                (if (listp car) car (list car)))
                       ,@(cdr clause)))))
               clauses)))

(defun nailgun-format-chunk (type payload)
  (bindat-pack
   nailgun-chunk-spec
   `((:length . ,(length payload))
     (:type . ,type)
     (:payload . ,payload))))

(defun nailgun-send-raw-chunk (nailgun type payload)
  (process-send-string nailgun (nailgun-format-chunk type payload)))

(defun nailgun-send-chunk (nailgun type payload)
  (nailgun-send-raw-chunk nailgun (nailgun-keyword-to-type type) payload))

(defun nailgun-send-working-directory (nailgun &optional directory)
  (nailgun-send-chunk nailgun :working-directory (or directory default-directory)))

(defun nailgun-send-command (nailgun command)
  (nailgun-send-chunk nailgun :command command))

(defun nailgun-send-exit (nailgun)
  (nailgun-send-chunk nailgun :exit nil))

(defun nailgun-send-argument (nailgun argument)
  (nailgun-send-chunk nailgun :argument argument))

;;; chunk reading

(defvar nailgun-chunk-read 0
  "Length of buffered chunk in NAILGUN buffer.")

(defvar nailgun-chunk-length nil
  "Length of chunk payload.")

(defvar nailgun-chunk-type nil
  "Type of the chunk.")

;; TODO: this is horrible
(defun nailgun-process-filter (nailgun string callback)
  (let ((length (length string)))
    (loop
     (when (eql length 0)
       (return))
     (unless (eql nailgun-chunk-read 0)
       (setf string (concat (with-current-buffer (process-buffer nailgun)
                              (prog1 (buffer-string)
                                (erase-buffer)))
                            string)
             length (+ length nailgun-chunk-read)
             nailgun-chunk-read 0))
     (cond
      (nailgun-chunk-type
       (cond
        ((>= length (+ nailgun-chunk-length nailgun-chunk-header-length))
         (let ((chunk-payload (substring string nailgun-chunk-header-length (+ nailgun-chunk-header-length nailgun-chunk-length)))
               (chunk-length nailgun-chunk-length)
               (chunk-type nailgun-chunk-type))
           (setf string (substring string (+ nailgun-chunk-header-length nailgun-chunk-length))
                 length (length string)
                 nailgun-chunk-length nil
                 nailgun-chunk-type nil)
           (funcall callback chunk-length chunk-type chunk-payload)))
        (t
         (with-current-buffer (process-buffer nailgun)
           (erase-buffer)
           (insert string)
           (setf nailgun-chunk-read length)
           (return)))))
      (nailgun-chunk-length
       (cond
        ((>= length nailgun-chunk-header-length)
         (let ((header (bindat-unpack nailgun-chunk-header-spec string)))
           (setf nailgun-chunk-type (cdr (assoc :type header)))))
        (t
         (with-current-buffer (process-buffer nailgun)
           (erase-buffer)
           (insert string)
           (setf nailgun-chunk-read length)
           (return)))))
      (t
       (cond
        ((>= length nailgun-chunk-length-length)
         (let* ((both (>= length nailgun-chunk-header-length))
                (header (bindat-unpack
                         (if both
                             nailgun-chunk-header-spec
                           nailgun-chunk-length-spec)
                         string)))
           (setf nailgun-chunk-length (cdr (assoc :length header)))
           (when both
             (setf nailgun-chunk-type (cdr (assoc :type header))))))
        (t
         (with-current-buffer (process-buffer nailgun)
           (erase-buffer)
           (insert string)
           (setf nailgun-chunk-read length)
           (return)))))))))

(provide 'nailgun)
;; EOF

;;; nailgun-eclim.el
(require 'nailgun)
(require 'json)

;; eclipse 1  <>  #<process nailgun 1>  <>  #<buffer *eclim 1*>
;;                         ^
;;                         v
;;                #<buffer *nailgun 1*>

;; is the error output even interesting?
;; in general put output for each file descriptor in separate result
;; buffers
;; if we need to reconnect, a new process is created, so we can't put
;; any information in there, better to put all session information into
;; a separate struct or associate it with the original process buffer

;; read bytes, buffer them until a full chunk arrived, maybe with the
;; filter functionality, then decode that chunk and dispatch on its type

(defun my-eclim-send-command-callback (callback command &rest args)
  (let ((nailgun (nailgun-connect "localhost" 9091 callback)))
    (unwind-protect
        (progn
          ;; TODO: buffer this stuff before sending it
          (nailgun-send-working-directory nailgun)
          (nailgun-send-argument nailgun "-command")
          (nailgun-send-argument nailgun command)
          (dolist (arg args)
            (nailgun-send-argument nailgun arg))
          (nailgun-send-command nailgun "org.eclim.command.Main")
          (loop (accept-process-output nailgun)))
      (nailgun-disconnect nailgun))))

(defun my-eclim-send-command (command &rest args)
  "Sends the command and assembles the decoded standard output into the *eclim* buffer."
  (with-current-buffer (get-buffer-create "*eclim*")
    (erase-buffer))
  (block my-eclim-send-command
    (flet ((callback (length type payload)
             (let ((string (decode-coding-string payload 'utf-8-dos t)))
               (nailgun-type-case type
                (:exit
                 (return-from my-eclim-send-command
                   (car (read-from-string string))))
                (:stdout
                 (with-current-buffer (get-buffer-create "*eclim*")
                   (insert string)))
                (:stderr
                 ;; TODO: read everything here
                 (error (decode-coding-string payload 'utf-8-dos t)))
                (t
                 (error "can't handle chunk type %s" (nailgun-type-to-keyword type)))))))
      (apply 'my-eclim-send-command-callback 'callback command args))))

(defun my-eclim-send-simple-command (command &rest args)
  "Sends the command and returns the first standard output chunk decoded from JSON."
  (block my-eclim-send-simple-command
    (apply
     'my-eclim-send-command-callback
     (lambda (length type payload)
       (nailgun-type-case type
        (:exit
         (return-from my-eclim-send-simple-command))
        (:stdout
         (return-from my-eclim-send-simple-command
           (decode-coding-string payload 'utf-8-dos t)))
        (:stderr
         ;; TODO: read everything here?
         (error (decode-coding-string payload 'utf-8-dos t)))
        (t
         (error "can't handle nailgun chunk type %s" (nailgun-type-to-keyword type)))))
     command args)))

(defun my-eclim-send-simple-json-command (command &rest args)
  (let ((payload (apply 'my-eclim-send-simple-command command args)))
    (and payload (json-read-from-string payload))))

(defun my-eclim-send-json-command (command &rest args)
  (when (eql (apply 'my-eclim-send-command command args) 0)
    (with-current-buffer (get-buffer-create "*eclim*")
      (json-read-from-string (buffer-string)))))

;;; commands

(defun my-eclim-list-commands ()
  (my-eclim-send-command "-?" "commands"))

(defun my-eclim-ping ()
  (my-eclim-send-simple-json-command "ping"))

(defun my-eclim-java-list-installs ()
  (my-eclim-send-simple-json-command "java_list_installs"))

(defun my-eclim-java-unit-tests (project)
  (my-eclim-send-json-command "java_junit_tests" "-p" project))

(defun my-eclim-project-info (project)
  (my-eclim-send-json-command "project_info" "-p" project))

(defun my-eclim-project-list (&optional nature)
  (apply 'my-eclim-send-json-command "project_list" (and nature (list "-n" nature))))

(defun my-eclim-project-natures (&optional project)
  (apply 'my-eclim-send-json-command "project_natures" (and project (list "-p" project))))

(defun my-eclim-jobs (&optional family)
  (apply 'my-eclim-send-json-command "jobs" (and family (list "-f" family))))

(defun my-eclim-history-add (project file)
  (my-eclim-send-json-command "history_add" "-p" project "-f" file))

(defun my-eclim-history-clear (project file)
  (my-eclim-send-json-command "history_clear" "-p" project "-f" file))

(defun my-eclim-history-list (project file)
  (my-eclim-send-json-command "history_list" "-p" project "-f" file))

(defun my-eclim-java-checkstyle (project file)
  (my-eclim-send-json-command "java_checkstyle" "-p" project "-f" file))

(defun my-eclim-java-classpath (project &optional delimiter)
  (apply 'my-eclim-send-json-command
         "java_classpath"
         "-p" project
         (and delimiter (list "-d" delimiter))))

(defun my-eclim-java-classpath-variables ()
  (my-eclim-send-json-command "java_classpath_variables"))

(defun my-eclim-java-complete (project file offset encoding layout)
  (my-eclim-send-json-command
   "-p" project
   "-f" file
   "-o" offset
   "-e" encoding
   "-l" layout))

(defun my-eclim-java-complete-package (project &optional name)
  (apply 'my-eclim-send-json-command
         "java_complete_package"
         "-p" project
         (and name (list "-n" name))))

(defun my-eclim-java-constructor (project file offset &optional encoding properties s)
  (apply 'my-eclim-send-command
         "java_constructor"
         "-p" project
         "-f" file
         "-o" offset
         `(,@(and encoding (list "-e" encoding))
           ,@(and properties (list "-r" properties))
           ,@(and s (list "-s")))))

(defun my-eclim-java-doc-url-open (url)
  (my-eclim-send-command "java_doc_url_open" "-u" url))

(defun my-eclim-projects ()
  (my-eclim-send-json-command "projects"))

(defun my-eclim-refactor_redo (&optional p)
  (apply 'my-eclim-send-command "refactor_redo" (and p '("-p"))))

(defun my-eclim-refactor_undo (&optional p)
  (apply 'my-eclim-send-command "refactor_undo" (and p '("-p"))))

(defun my-eclim-reload ()
  (message (substring (my-eclim-send-simple-command "reload") 1 -1)))

(defun my-eclim-setting (setting)
  (my-eclim-send-json-command "setting" "-s" setting))

(defun my-eclim-settings ()
  (my-eclim-send-json-command "settings"))

;; FIXME: throws exception
(defun my-eclim-settings-update (&optional settings)
  (my-eclim-send-command "settings_update"))

(defun my-eclim-shutdown ()
  (my-eclim-send-simple-command "shutdown"))

;; webxml_validate -p project -f file

(defun my-eclim-workspace-dir ()
  (my-eclim-send-simple-command "workspace_dir"))

;; TODO: reuse connection, restart connection, automatically connect
;; TODO: different connections, i.e. multiple buffers/processes
;; TODO: allow for different encoding