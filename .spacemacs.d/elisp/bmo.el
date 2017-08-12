(defvar bz-url "http://bugzilla.vm/")
(defvar bz-dir "/opt/bugzilla")

(defun bz-browse-site ()
  (interactive)
  (browse-url (concat bz-url (projectile-default-project-name (projectile-project-root)))))

(defun bz-browse-bug ()
  (interactive)
  (let ((bug-id (projectile-default-project-name (projectile-project-root))))
    (when (string-match "^[0-9]+$" bug-id)
      (browse-url (concat  "https://bugzilla.mozilla.org/show_bug.cgi?id=" bug-id)))))

(defun bz-new (bug-id)
  "start working on a new bug"
  (interactive "s")
  (let ((default-directory bz-dir))
    (async-shell-command (format "bz new %s" bug-id) (format "*bznew:%s*" bug-id))))

(defun bz-list ()
  (mapcar #'car
          (remove-if-not (lambda (x) (and (cadr x) (not (or (equal (car x) "..") (equal (car x) ".")))))
                         (directory-files-and-attributes (concat bz-dir "/htdocs")))))

(defun bz-bug-id-p (bug-id) (not (null (string-match "^[0-9]+$" bug-id))))
(defun bz-list-bugs () (remove-if-not #'bz-bug-id-p (bz-list)))

(defun bz-summary ()
  "Show summary for current bug in projectile root"
  (interactive)
  (message (f-read (concat (projectile-project-root) "/data/summary"))))

(defun bz-checksetup ()
  "Run checksetup.pl in the current project"
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
                               (async-shell-command "perl checksetup.pl")))

(defun bz-goto (bug-dir &optional switches)
  (interactive (let ((default-directory (f-join bz-dir "htdocs/")))
                 (dired-read-dir-and-switches "")))
  (switch-to-buffer (dired-noselect bug-dir switches)))

(defun bmo-summary (bug-id)
  (let ((response (request (format "https://bugzilla.mozilla.org/rest/bug/%s" bug-id)
                           :params '( ("include_fields" . "summary") )
                           :parser 'json-read
                           :sync t)))
    (cdr (assq 'summary (aref (cdr (assq 'bugs (request-response-data response))) 0)))))

(defun my-org-describe-link (link description)
  (cond ((string-match "^bmo:\\([0-9]+\\)" link)
         (let ((bug-id (match-string 1 link)))
           (format "Bug %s - %.75s" bug-id (bmo-summary bug-id))))
        (t (or description link))))

(defun my-org-open-bmo (bug-id)
  (browse-url (format "https://bugzilla.mozilla.org/show_bug.cgi?id=%s" bug-id)))

(defun my-org-format-bmo (bug-id desc format)
  (case format ('html
                (format "<a href='https://bugzilla.mozilla.org/show_bug.cgi?id=%s'>%s</a>" bug-id desc))
        (otherwise (format "[%s]" desc))))


(setq org-make-link-description-function #'my-org-describe-link)

(provide 'bmo)
