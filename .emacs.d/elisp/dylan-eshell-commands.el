(defun eshell/clear ()
  "Clears the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/ag (pattern)
  (ag pattern default-directory))

(defun eshell/ff (&rest args)
  "Opens a file in emacs."
  (unless (null args)
    (--each (eshell-flatten-list (reverse args))
      (find-file-other-window it))))

(defun eshell/bookmark (name &optional location)
  (bookmark-set name)
  (when location
    (bookmark-set-filename name location)))

(defun eshell/bookmarks ()
  (let ((max-width (apply #'max (mapcar (lambda (x) (length (car x))) bookmark-alist))))
    (dolist (bookmark bookmark-alist)
      (eshell-printn
       (format (format "%%-%ds (%%s)" max-width)
               (car bookmark)
               (cdr (assq 'filename (cdr bookmark))))))))

(defun around-eshell/cd (orig &rest args)
  (if (and args (assoc (car args) bookmark-alist))
      (apply orig (cons (bookmark-location (car args)) (cdr args)))
    (apply orig args)))

(advice-add 'eshell/cd :around #'around-eshell/cd)

(defun eshell/cdp ()
  (eshell/cd (projectile-project-root)))

(defun eshell/cdb (str)
  (let ((new-dir
         (let* ((default-directory bz-dir)
                (bug-dir (shell-command-to-string (format "bz path '%s'" str))))
           (with-parsed-tramp-file-name default-directory nil
             (tramp-make-tramp-file-name method user host bug-dir hop)))))
    (eshell/cd new-dir)
    (shell-command-to-string "bz summary")))

(provide 'dylan-eshell-commands)