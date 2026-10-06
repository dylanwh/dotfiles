;;; ssh-auth-sock --- some things to help with ssh from inside emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'bookmark)

(defvar ssh-auth-sock--patterns
  (cond
   ((eq system-type 'darwin)
    (list "/private/tmp/com.apple.launchd.*/Listeners"))
   (t
    (list "/tmp/ssh-*/agent.*" (expand-file-name "~/.ssh/agent/s.*.sshd.*")))))

(defun ssh-auth-sock--files ()
  "Return the paths of all ssh auth sockets."
  (cons (expand-file-name "~/.bitwarden-ssh-agent.sock")
        (mapcan #'file-expand-wildcards ssh-auth-sock--patterns)))

(defun ssh-auth-sock--usable-p (path-attrs)
  "Return t if PATH-ATTRS is a reachable socket owned by current user id."
  (and (cdr path-attrs)
       (ssh-auth-sock--socket-p (cdr path-attrs))
       (ssh-auth-sock--reachable-p (car path-attrs))
       (= (file-attribute-user-id (cdr path-attrs)) (user-uid))))

(defun ssh-auth-sock--discover ()
  "Return the paths of owned sockets, newest first."
  (let* ((path-attrs (mapcar (lambda (path) (cons path (file-attributes path))) (ssh-auth-sock--files)))
         (usable (cl-remove-if-not #'ssh-auth-sock--usable-p path-attrs))
         (sorted (cl-sort usable #'>
                          :key (lambda (pa)
                                 (float-time
                                  (file-attribute-modification-time
                                   (cdr pa)))))))
    (mapcar #'car sorted)))

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
  "Return the SSH_AUTH_SOCK path or nil."
  (let ((sockets (ssh-auth-sock--discover)))
    (car sockets)))

(defun ssh-auth-sock-update ()
  "Update the SSH_AUTH_SOCK environment variable.

  Sets SSH_AUTH_SOCK to the value returned by `ssh-auth-sock', synchronizing
  Emacs's environment with the current SSH agent socket.  Useful when the SSH
  agent socket path has changed, such as after reattaching to a tmux session."
  (interactive)
  (let ((sock (ssh-auth-sock)))
    (message "Set SSH_AUTH_SOCK to %s" sock)
    (setenv "SSH_AUTH_SOCK" sock)))

(provide 'ssh-auth-sock)
;;; ssh-auth-sock.el ends here
