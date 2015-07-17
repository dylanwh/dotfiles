(defvar bz-url "http://bugzilla.local/")
(defvar bz-dir "/ssh:bugzilla:/opt/bugzilla")

(defun eshell/cdb (bug-id)
  (cdb bug-id)
  (rename-buffer (format "*eshell: %s*" bug-id))
  (shell-command-to-string "bz summary"))

(defun cdb (bug-id)
  (interactive (list
                (read-string (format "Bug (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
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
  (interactive "sBug ")
  (let ((default-directory bz-dir))
    (async-shell-command (concat "bz new " bug-id) "*bznew*")))

(provide 'bmo)
