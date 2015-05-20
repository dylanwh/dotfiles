(defun eshell/cdb (path &rest args)
  (let ((bz-dir (shell-command-to-string (format "bz path %s" path))))
    (if (file-remote-p default-directory)
        (with-parsed-tramp-file-name default-directory nil
          (eshell/cd (tramp-make-tramp-file-name method user host bz-dir hop)))
      (eshell/cd bz-dir)))
  (shell-command-to-string "bz summary"))

(after 'projectile
  (defun bz-go ()
    (interactive)
    (browse-url (concat  "http://localhost/" (projectile-project-name))))

  (defun bz-visit-bug ()
    (interactive)
    (let ((bug-id (projectile-project-name)))
      (when (string-match "^[0-9]+$" bug-id)
        (browse-url (concat  "https://bugzilla.mozilla.org/show_bug.cgi?id=" bug-id))))))

(provide 'bmo)
