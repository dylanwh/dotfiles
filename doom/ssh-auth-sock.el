;;; ssh-auth-sock --- some things to help with ssh from inside emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:
(require 'cl-lib)

(require 'bookmark)

(cl-defstruct (ssh-auth-sock
               (:constructor make-ssh-auth-sock (&key path (attr (file-attributes path)))))
  path attr)

(defun ssh-auth-sock--recent (paths)
  "Return a list of ssh-auth-socks from PATHS."
  (cl-sort (cl-loop for p in paths
                    collect (make-ssh-auth-sock :path p))
           #'>
           :key (lambda (s)
                  (float-time
                   (file-attribute-modification-time
                    (ssh-auth-sock-attr s))))))

(defun ssh-auth-sock--rbw ()
  "Return the path of rbw's ssh-agent."
  (if-let* ((runtime_dir (getenv "XDG_RUNTIME_DIR")))
      (make-ssh-auth-sock :path (file-name-concat runtime_dir "rbw/ssh-agent-socket"))
    (make-ssh-auth-sock
     (file-name-concat (temporary-file-directory)
                       (format "rbw-%d" (user-uid))
                       "ssh-agent-socket"))))

(defun ssh-auth-sock--bitwarden ()
  "Return the path of bitwarden's ssh-agent."
  (make-ssh-auth-sock :path (expand-file-name "~/.bitwarden-ssh-agent.sock")))

(defun ssh-auth-sock--secretive ()
  "Return the path of secretive's ssh-agent socket."
  (make-ssh-auth-sock :path (expand-file-name "~/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh")))

(defun ssh-auth-sock--launchd ()
  "Return the paths of launchd's ssh-agent sockets."
  (ssh-auth-sock--recent (cl-concatenate 'list (file-expand-wildcards "/var/run/com.apple.launchd.*/Listeners")
                                         (file-expand-wildcards "/private/tmp/com.apple.launchd.*/Listeners"))))

(defun ssh-auth-sock--openssh ()
  "Return the paths of OpenSSH agent sockets, local and forwarded."
  (ssh-auth-sock--recent  (cl-concatenate 'list (file-expand-wildcards (expand-file-name "~/.ssh/agent/s.*"))
                                          (file-expand-wildcards "/tmp/ssh-*/agent.*"))))

(defun ssh-auth-sock--files ()
  "Return the paths of all ssh auth sockets."
  (let ((ssh-auth-socks (cl-concatenate 'list (if (eq system-type 'darwin)
                                                  (cons (ssh-auth-sock--secretive) (ssh-auth-sock--launchd))
                                                (list (ssh-auth-sock--rbw)
                                                      (ssh-auth-sock--bitwarden)))
                                        (ssh-auth-sock--openssh))))
    (cl-loop for s in ssh-auth-socks
             when (ssh-auth-sock--usable-p s)
             collect (ssh-auth-sock-path s))))


(defun ssh-auth-sock--usable-p (s)
  "Return t if S is a reachable socket owned by current user id."
  (and (ssh-auth-sock-attr s)
       (ssh-auth-sock--socket-p (ssh-auth-sock-attr s))
       (ssh-auth-sock--reachable-p (ssh-auth-sock-path s))
       (= (file-attribute-user-id (ssh-auth-sock-attr s)) (user-uid))))

(defun ssh-auth-sock--socket-p (attributes)
  "Return t if mode of ATTRIBUTES indicates socket."
  (and (null (file-attribute-type attributes))
       (eq (aref (file-attribute-modes attributes) 0) ?s)))

(defun ssh-auth-sock--reachable-p (path)
  "Return t if a Unix connection can be established to PATH."
  (when (file-exists-p path)
    (let ((proc (ignore-errors
                  (make-network-process
                   :name "socket-test"
                   :service path
                   :family 'local
                   :nowait nil))))
      (when proc
        (delete-process proc)
        t))))

(defun ssh-auth-sock ()
  "Return the SSH_AUTH_SOCK path or nil."
  (let ((sockets (ssh-auth-sock--files)))
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
