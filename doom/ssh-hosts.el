;;; doom/ssh-hosts.el -*- lexical-binding: t; -*-

(defun ssh-hosts--parse-file (file visited)
  "Parse FILE for Host aliases and Include directives.
VISITED is a list of already-parsed file truenames to prevent cycles.
Returns a list of host alias strings."
  (let ((truename (file-truename file)))
    (when (and (file-readable-p file)
               (not (member truename visited)))
      (let ((visited (cons truename visited))
            (hosts '()))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (string-trim (thing-at-point 'line t))))
              (cond
               ((or (string-empty-p line)
                    (string-prefix-p "#" line)))
               ((string-match "^Include[[:space:]]+\\(.*\\)$" line)
                (let* ((pattern (match-string 1 line))
                       (expanded (substitute-in-file-name
                                  (expand-file-name pattern (file-name-directory file))))
                       (files (file-expand-wildcards expanded t)))
                  (dolist (f files)
                    (setq hosts (append (ssh-hosts--parse-file f visited) hosts)))))
               ((string-match "^Host[[:space:]]+\\(.*\\)$" line)
                (let ((aliases (split-string (match-string 1 line))))
                  (dolist (alias aliases)
                    (unless (string-match-p "[*?]" alias)
                      (push alias hosts)))))))
            (forward-line 1)))
        (nreverse hosts)))))

(defun ssh-hosts (&optional file-name)
  "Return a list of Host aliases defined in FILE-NAME and its Includes.
Wildcards and GitHub/Heroku hosts are excluded. Duplicates are removed."
  (let ((config (expand-file-name (or file-name "~/.ssh/config"))))
    (if (file-readable-p config)
        (seq-remove (lambda (host)
                      (string-match-p (rx (or "github.com" "heroku.com")) host))
                    (delete-dups (ssh-hosts--parse-file config '())))
      (user-error "Cannot read %s" config))))
