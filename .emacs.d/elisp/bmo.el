(defvar bz-url "http://bugzilla.local/")
(defvar bz-dir "/ssh:bugzilla:/opt/bugzilla")

(defun eshell/cdb (bug-id)
  (cdb bug-id)
  (shell-command-to-string "bz summary"))

(defun bzshell ()
  (interactive)
  (let ((default-directory bz-dir)
        (eshell-buffer-name "*bzshell*"))
    (eshell)))

(defun cdb (bug-id)
  (interactive "s")
  (cd bz-dir)
  (let ((bug-dir (shell-command-to-string (format "bz path %s" bug-id))))
    (with-parsed-tramp-file-name default-directory nil
      (message default-directory)
      (cd (tramp-make-tramp-file-name method user host bug-dir hop)))))

(after 'projectile
  (defun bz-browse-site ()
    (interactive)
    (browse-url (concat bz-url (projectile-project-name))))

  (defun bz-browse-bug ()
    (interactive)
    (let ((bug-id (projectile-project-name)))
      (when (string-match "^[0-9]+$" bug-id)
        (browse-url (concat  "https://bugzilla.mozilla.org/show_bug.cgi?id=" bug-id))))))

(defun bz-new (bug-id)
  "start working on a new bug"
  (interactive "s")
  (let ((default-directory bz-dir))
    (async-shell-command (format "yes | bz new %s" bug-id) (format "*bznew:%s*" bug-id))))

(defun bmo-sql ()
  (interactive)

  (let* ((auth-result (car (auth-source-search
                            :host "bugzilla.local"
                            :port "mysql")))
         (sql-connection-alist
          `((bmo (sql-product 'mysql)
                 (sql-server "bugzilla.local")
                 (sql-user ,(plist-get auth-result ':user))
                 (sql-database "bugs_bmo")
                 (sql-password ,(funcall (plist-get auth-result ':secret)))))))
    (sql-connect 'bmo "bmo-sql")))

(defun bz-bug-id-p (bug-id) (not (null (string-match "^[0-9]+$" bug-id))))

(defun bz-list ()
  (mapcar #'car
          (remove-if-not (lambda (x) (and (cadr x) (not (or (equal (car x) "..") (equal (car x) ".")))))
                         (directory-files-and-attributes (concat bz-dir "/htdocs")))))

(defun bz-list-bugs () (remove-if-not #'bz-bug-id-p (bz-list)))

(defun bz-summary ()
  (interactive)
  (message (f-read (concat (projectile-project-root) "/data/summary"))))

(defun bmo-summary (bug-id)
  (let ((bug-dir (f-join bz-dir (format "htdocs/%s" bug-id))))
    (if (f-dir? bug-dir)
        (f-read (f-join bug-dir "data" "summary"))
      (let ((response (request (format "https://bugzilla.mozilla.org/rest/bug/%s" bug-id)
                                  :params '( ("include_fields" . "summary") )
                                  :parser 'json-read
                                  :sync t)))
        (cdr (assq 'summary (aref (cdr (assq 'bugs (request-response-data response))) 0)))))))

(provide 'bmo)
