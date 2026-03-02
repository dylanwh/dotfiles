;;; ssh-utils --- some things to help with ssh from inside emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'terminal-here)
(require 'ivy)
(require 'bookmark)

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



(defvar ssh-auth-sock--patterns
  (cond
   ((eq system-type 'darwin)
    (list "/private/tmp/com.apple.launchd.*/Listeners"))
   (t
    (list "/tmp/ssh-*/agent.*" (expand-file-name "~/.ssh/agent/s.*.sshd.*")))))

(defun ssh-auth-sock--discover ()
  "Return the paths of owned sockets, newest first."
  (let* ((uid (user-uid))
         (pairs (mapcar (lambda (path) (cons path (file-attributes path)))
                        (mapcan #'file-expand-wildcards ssh-auth-sock--patterns)))
         (socks (cl-remove-if-not
                 (lambda (pair)
                   (and (cdr pair)
                        (ssh-auth-sock--socket-p (cdr pair))
                        (ssh-auth-sock--reachable-p (car pair))
                        (= (file-attribute-user-id (cdr pair)) uid)))
                 pairs))
         (sorted (cl-sort socks #'>
                          :key (lambda (pair)
                                 (float-time
                                  (file-attribute-modification-time
                                   (cdr pair)))))))
    (mapcar #'car sorted)))



(defvar ssh-auth-sock--op-agent (expand-file-name "~/.1password/agent.sock"))

(defun ssh-auth-sock--socket-p (attributes)
  "Return t if mode of ATTRIBUTES indicates socket."
  (and (null (file-attribute-type attributes))
       (eq (aref (file-attribute-modes attributes) 0) ?s)))

(defun ssh-auth-sock--reachable-p (socket-path)
  "Return t if a Unix connection can be established to SOCKET-PATH."
  (when (file-exists-p socket-path)
    (let ((proc (ignore-errors
                  (make-network-process
                   :name "socket-test"
                   :service socket-path
                   :family 'local
                   :nowait nil))))
      (when proc
        (delete-process proc)
        t))))

(defun ssh-auth-sock ()
  "Return the SSH_AUTH_SOCK path, or 1password auth socket, or nil."
  (let ((sockets (ssh-auth-sock--discover)))
    (if (and
         (null sockets)
         (ssh-auth-sock--socket-p (file-attributes ssh-auth-sock--op-agent))
         (ssh-auth-sock--reachable-p ssh-auth-sock--op-agent))
        ssh-auth-sock--op-agent
      (car sockets))))

(defun eshell/ssh (host)
  "Open an external terminal with SSH connection to HOST."
  (terminal-here-launch (list "kitten" "ssh" host)))

(put 'eshell/ssh 'eshell-arguments-complete '(ssh-hosts))

(defun ssh-update-auth ()
  "Update the SSH_AUTH_SOCK environment variable.

  Sets SSH_AUTH_SOCK to the value returned by `ssh-auth-sock', synchronizing
  Emacs's environment with the current SSH agent socket.  Useful when the SSH
  agent socket path has changed, such as after reattaching to a tmux session."
  (interactive)
  (let ((sock (ssh-auth-sock)))
    (message "Set SSH_AUTH_SOCK to %s" sock)
    (setenv "SSH_AUTH_SOCK" sock)))

(defun ssh-terminal (host)
  "Connect to HOST via SSH in an external terminal."
  (interactive (list (ivy-completing-read "Host: " (ssh-hosts))))
  (terminal-here-launch (list "kitten" "ssh" host)))


(defun ssh-register-bookmarks ()
  "Register a TRAMP bookmark for each SSH host."
  (interactive)
  (dolist (host (ssh-hosts))
    (bookmark-store
     (concat "ssh:" host)
     `((filename . ,(concat "/ssh:" host ":~/")))
     nil)))

(provide 'ssh-utils)
;;; ssh-utils.el ends here
