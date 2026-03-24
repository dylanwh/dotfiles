;;; shellfish.el --- Secure ShellFish iOS terminal integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Port of ShellFish shell functions to Emacs Lisp.
;; These communicate with the Secure ShellFish iOS terminal via OSC escape sequences.

;;; Code:

(defun shellfish--base64 (string)
  "Return base64 encoding of STRING with no newlines."
  (base64-encode-string (encode-coding-string string 'utf-8) t))

(defun shellfish--send-sequence (payload)
  "Send ShellFish OSC escape sequence with PAYLOAD to the terminal.
Wraps in DCS passthrough when running under tmux."
  (let ((seq (if (getenv "TMUX")
                 (format "\033Ptmux;\033\033]%s\a\033\\" payload)
               (format "\033]%s\a" payload))))
    (send-string-to-terminal seq (frame-terminal))))

(defun shellfish--prepare-result ()
  "Create a FIFO for receiving results from ShellFish.
Returns the FIFO path."
  (let ((path (make-temp-name (expand-file-name "shellfish-" temporary-file-directory))))
    (call-process "mkfifo" nil nil nil path)
    path))

(defun shellfish--decode-reply (reply)
  "Decode a ShellFish REPLY string.
Returns the decoded result, or signals an error."
  (cond
   ((string-prefix-p "error=" reply)
    (error "%s" (decode-coding-string
                 (base64-decode-string (substring reply 6))
                 'utf-8)))
   ((string-prefix-p "result=" reply)
    (decode-coding-string
     (base64-decode-string (substring reply 7))
     'utf-8))
   (t reply)))

(defun shellfish--handle-result (fifo-path &optional callback)
  "Read result from FIFO-PATH asynchronously and clean up.
If CALLBACK is non-nil, call it with the decoded result.
If CALLBACK is nil, report errors via `message'."
  (let ((buf (generate-new-buffer " *shellfish-result*")))
    (make-process
     :name "shellfish-result"
     :buffer buf
     :command (list "cat" fifo-path)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((reply (with-current-buffer buf
                            (buffer-string))))
               (condition-case err
                   (let ((result (shellfish--decode-reply reply)))
                     (when callback
                       (funcall callback result)))
                 (error (message "ShellFish error: %s" (error-message-string err)))))
           (delete-file fifo-path)
           (kill-buffer buf)))))))

(defun shellfish--handle-result-sync (fifo-path)
  "Read result from FIFO-PATH synchronously and clean up.
Returns the decoded result string, or signals an error.
Blocks until ShellFish writes to the FIFO."
  (unwind-protect
      (shellfish--decode-reply
       (with-temp-buffer
         (insert-file-contents fifo-path)
         (buffer-string)))
    (delete-file fifo-path)))

(defun shellfish-open-url (url &optional _new-window)
  "Open URL on iOS via ShellFish terminal escape sequence.
Compatible with `browse-url-browser-function'."
  (interactive "sURL: ")
  (let* ((fifo (shellfish--prepare-result))
         (payload (concat "6;open://?ver=2"
                          "&respond=" (shellfish--base64 fifo)
                          "&url=" (shellfish--base64 url))))
    (shellfish--send-sequence payload)
    (shellfish--handle-result fifo)))

(defun shellfish-quicklook (files &optional text)
  "Show QuickLook preview for FILES on iOS via ShellFish.
FILES is a list of file paths. If FILES is nil, send TEXT instead.
Interactively, previews the current buffer's file, or sends buffer
contents if the buffer has no file."
  (interactive
   (if (buffer-file-name)
       (list (list (buffer-file-name)))
     (list nil (buffer-substring-no-properties (point-min) (point-max)))))
  (let* ((fifo (shellfish--prepare-result))
         (payload (concat "6;quicklook://?ver=2"
                          "&respond=" (shellfish--base64 fifo)
                          "&pwd=" (shellfish--base64 default-directory)
                          "&home=" (shellfish--base64 (expand-file-name "~"))
                          (mapconcat (lambda (f)
                                       (concat "&path=" (shellfish--base64
                                                         (expand-file-name f))))
                                     (or files '()) "")
                          (if text
                              (concat "&text=" (shellfish--base64 text))
                            ""))))
    (shellfish--send-sequence payload)
    (shellfish--handle-result fifo)))

(defun shellfish-paste ()
  "Paste from iOS clipboard via ShellFish and insert at point."
  (interactive)
  (let* ((fifo (shellfish--prepare-result))
         (payload (concat "6;pbpaste://?ver=2"
                          "&respond=" (shellfish--base64 fifo))))
    (shellfish--send-sequence payload)
    (shellfish--handle-result
     fifo (lambda (text) (insert text)))))

(defun shellfish--paste-sync ()
  "Return iOS clipboard contents as a string.  Blocks."
  (let* ((fifo (shellfish--prepare-result))
         (payload (concat "6;pbpaste://?ver=2"
                          "&respond=" (shellfish--base64 fifo))))
    (shellfish--send-sequence payload)
    (shellfish--handle-result-sync fifo)))

(defun eshell/shellfish-paste ()
  "Paste from iOS clipboard in eshell."
  (shellfish--paste-sync))

(defun shellfish-sharesheet (files &optional text)
  "Present iOS share sheet for FILES via ShellFish.
FILES is a list of file paths. If FILES is nil, share TEXT instead.
Interactively, shares the current buffer's file, or the buffer
contents if the buffer has no file."
  (interactive
   (if (buffer-file-name)
       (list (list (buffer-file-name)))
     (list nil (buffer-substring-no-properties (point-min) (point-max)))))
  (let* ((fifo (shellfish--prepare-result))
         (payload (concat "6;sharesheet://?ver=2"
                          "&respond=" (shellfish--base64 fifo)
                          "&pwd=" (shellfish--base64 default-directory)
                          "&home=" (shellfish--base64 (expand-file-name "~"))
                          (mapconcat (lambda (f)
                                       (concat "&path=" (shellfish--base64
                                                         (expand-file-name f))))
                                     (or files '()) "")
                          (if text
                              (concat "&text=" (shellfish--base64 text))
                            ""))))
    (shellfish--send-sequence payload)
    (shellfish--handle-result fifo)))

(defun shellfish-run-shortcut (name &optional input x-callback)
  "Run iOS Shortcut NAME with optional INPUT via ShellFish.
If X-CALLBACK is non-nil, use x-callback-url to return results."
  (interactive "sShortcut name: ")
  (let ((base-url (if x-callback
                      "shortcuts://x-callback-url/run-shortcut"
                    "shortcuts://run-shortcut")))
    (shellfish-open-url
     (concat base-url
             "?name=" (url-hexify-string name)
             (when input
               (concat "&input=" (url-hexify-string input)))))))

(defun shellfish-snip (text)
  "Add TEXT as a ShellFish keyboard bar snippet."
  (interactive "sSnippet text: ")
  (shellfish--send-sequence
   (concat "6;addsnippet://?ver=2&text=" (shellfish--base64 text))))

(defun shellfish-set-bar-color (color)
  "Set ShellFish terminal toolbar to COLOR (a CSS-style color string)."
  (interactive "sColor: ")
  (shellfish--send-sequence
   (concat "6;settoolbar://?ver=2&color=" (shellfish--base64 color))))

(defun shellfish--sync-bar-color (&rest _)
  "Set ShellFish toolbar color to match the current mode-line background."
  (let ((color (face-background 'mode-line nil t)))
    (when color
      (shellfish-set-bar-color color))))

(add-hook 'enable-theme-functions #'shellfish--sync-bar-color)

;;; Push notification support (widget & notify)

(defun shellfish--push-credentials ()
  "Return (USER . KEY) from auth-source for host \"shellfish\"."
  (pcase-let ((`(,user ,key) (auth-source-user-and-password "shellfish")))
    (unless (and user key)
      (error "No auth-source entry found for host \"shellfish\""))
    (cons user key)))

(defun shellfish--random-iv ()
  "Return 32 hex characters (16 random bytes)."
  (with-temp-buffer
    (call-process "openssl" nil t nil "rand" "-hex" "16")
    (string-trim (buffer-string))))

(defun shellfish--upload-encrypted-image (image-path key iv user)
  "Encrypt IMAGE-PATH with KEY and image IV derived from IV, upload to ShellFish.
Returns the attachment ID string."
  (let ((image-iv (with-temp-buffer
                    (call-process "sh" nil t nil "-c"
                                  (format "printf '%s' | xxd -r -p | openssl dgst -sha256 -binary | head -c 16 | xxd -p" iv))
                    (string-trim (buffer-string))))
        (enc-file (make-temp-file "shellfish-img")))
    (unwind-protect
        (progn
          (call-process "openssl" nil nil nil
                        "enc" "-aes-256-cbc"
                        "-K" key "-iv" image-iv
                        "-in" (expand-file-name image-path)
                        "-out" enc-file)
          (with-temp-buffer
            (call-process "curl" nil t nil
                          "-sS" "-X" "POST"
                          "-H" "Content-Type: application/octet-stream"
                          "--data-binary" (concat "@" enc-file)
                          (format "https://secureshellfish.app/push/?attach=%s" user))
            (string-trim (buffer-string))))
      (delete-file enc-file))))

(defun shellfish--encrypt-and-post (header args url-suffix &optional image-path)
  "Encrypt HEADER and ARGS, POST to secureshellfish.app with URL-SUFFIX.
HEADER is e.g. \"Secure ShellFish Notify 3.0\".
ARGS is a list of strings to include as base64 lines in the plaintext.
If IMAGE-PATH is non-nil, encrypt and upload the image first."
  (let* ((creds (shellfish--push-credentials))
         (user (car creds))
         (key (cdr creds))
         (iv (shellfish--random-iv))
         (timestamp (format-time-string "%s"))
         (attach-id (when image-path
                      (shellfish--upload-encrypted-image image-path key iv user)))
         (all-args (if attach-id
                       (append (list "--image" attach-id) args)
                     args))
         (plain (concat header "\n"
                        timestamp "\n"
                        (mapconcat (lambda (arg)
                                     (base64-encode-string
                                      (encode-coding-string arg 'utf-8) t))
                                   all-args "\n")
                        "\n"))
         (plain-file (make-temp-file "shellfish-plain"))
         (enc-file (make-temp-file "shellfish-enc")))
    (unwind-protect
        (progn
          (with-temp-file plain-file
            (insert plain))
          (call-process "openssl" nil nil nil
                        "enc" "-aes-256-cbc"
                        "-K" key "-iv" iv
                        "-in" plain-file
                        "-out" enc-file)
          (let* ((payload (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (let ((coding-system-for-read 'binary))
                              (call-process "sh" nil t nil "-c"
                                            (format "printf '%s' | xxd -r -p" iv)))
                            (let ((coding-system-for-read 'binary))
                              (insert-file-contents-literally enc-file))
                            (base64-encode-string (buffer-string) t)))
                 (url (format "https://secureshellfish.app/push/?%s" url-suffix)))
            (let ((url-request-method "POST")
                  (url-request-extra-headers
                   '(("Content-Type" . "text/plain")))
                  (url-request-data payload))
              (url-retrieve url
                            (lambda (status)
                              (when-let ((err (plist-get status :error)))
                                (message "ShellFish push error: %s" err))
                              (kill-buffer))
                            nil t t))))
      (delete-file plain-file)
      (delete-file enc-file))))

(defun shellfish-notify (&rest args)
  "Send a push notification to ShellFish.
ARGS are strings: typically title and body text.
Keyword arguments:
  :image PATH       - encrypt and include an image
  :url URL          - open URL when notification is tapped
  :shortcut NAME    - run Shortcut when notification is tapped
Example: (shellfish-notify :url \"https://example.com\" \"Title\" \"Body\")"
  (interactive "sTitle: \nsBody: ")
  (let (image content)
    (if (called-interactively-p 'any)
        (setq content (list (nth 0 args) (nth 1 args)))
      (let ((rest args))
        (while rest
          (cond
           ((eq (car rest) :image)
            (setq image (cadr rest) rest (cddr rest)))
           ((eq (car rest) :url)
            (push "--url" content)
            (push (cadr rest) content)
            (setq rest (cddr rest)))
           ((eq (car rest) :shortcut)
            (push "--shortcut" content)
            (push (cadr rest) content)
            (setq rest (cddr rest)))
           (t
            (push (car rest) content)
            (setq rest (cdr rest)))))
        (setq content (nreverse content))))
    (shellfish--encrypt-and-post
     "Secure ShellFish Notify 3.0"
     content
     (format "user=%s&mutable"
             (car (shellfish--push-credentials)))
     image)))

(defvar shellfish--widget-type-keywords
  '(:text :progress :icon :color :url :shortcut)
  "Keywords that force the type of the next content value in widget args.")

(defun shellfish-widget (&rest args)
  "Update a ShellFish lock/home screen widget.
ARGS are content strings intermixed with keyword arguments.
Keyword arguments:
  :target TARGET       - direct content to a specific widget instance
  :image PATH          - encrypt and include an image file
  :text VALUE          - force VALUE as text
  :progress VALUE      - progress (e.g. \"50%\" or \"110/220\")
  :icon VALUE          - SF Symbol name (e.g. \"globe\")
  :color VALUE         - hex color for subsequent content
  :url VALUE           - URL opened when tapping widget
  :shortcut VALUE      - Shortcut to run when tapping widget
Bare strings are auto-detected by ShellFish.
Example: (shellfish-widget :target \"todos\" :icon \"checklist\" :progress \"3/10\")"
  (interactive "sWidget content: ")
  (let (target image content)
    (if (called-interactively-p 'any)
        (setq content (list (nth 0 args)))
      (let ((rest args))
        (while rest
          (cond
           ((eq (car rest) :target)
            (setq target (cadr rest) rest (cddr rest)))
           ((eq (car rest) :image)
            (setq image (cadr rest) rest (cddr rest)))
           ((memq (car rest) shellfish--widget-type-keywords)
            (push (concat "--" (substring (symbol-name (car rest)) 1)) content)
            (push (cadr rest) content)
            (setq rest (cddr rest)))
           (t
            (push (car rest) content)
            (setq rest (cdr rest)))))
        (setq content (nreverse content))))
    (shellfish--encrypt-and-post
     "Secure ShellFish Widget 3.0"
     (if target
         (append (list "--target" target) content)
       content)
     (format "user=%s"
             (car (shellfish--push-credentials)))
     image)))


(defvar shellfish--eshell-widget-options
  '("--target" "--image" "--text" "--progress" "--icon" "--color" "--url" "--shortcut")
  "Options recognized by eshell/widget that take a value argument.")

(defun eshell/widget (&rest args)
  "Update a ShellFish widget from eshell.
Translates --options to keyword args for `shellfish-widget'.
Example: widget --target todos --icon globe --progress 3/10 hello"
  (if (member "--help" args)
      (eshell-printn "Usage: widget [OPTIONS] [CONTENT]...

Options:
  --target TARGET    direct content to a specific widget instance
  --image PATH       encrypt and include an image file
  --text VALUE       force VALUE as text
  --progress VALUE   progress (e.g. 50% or 110/220)
  --icon VALUE       SF Symbol name (e.g. globe, terminal.fill)
  --color VALUE      hex color for subsequent content
  --url VALUE        URL opened when tapping widget
  --shortcut VALUE   Shortcut to run when tapping widget
  --help             show this help

Bare strings are auto-detected by ShellFish as text, progress,
icon, url, or color based on their format.")
    (let (result)
      (while args
        (if (member (car args) shellfish--eshell-widget-options)
            (let ((kw (intern (concat ":" (substring (car args) 2)))))
              (push kw result)
              (push (cadr args) result)
              (setq args (cddr args)))
          (push (car args) result)
          (setq args (cdr args))))
      (apply #'shellfish-widget (nreverse result)))))

(defun shellfish--ios-file-handler (operation &rest args)
  "File name handler for /ios/clipboard virtual file."
  (cond
   ((eq operation 'insert-file-contents)
    (let ((text (shellfish--paste-sync)))
      (insert text)
      (list (car args) (length text))))
   ((memq operation '(file-exists-p file-readable-p file-regular-p))
    t)
   ((eq operation 'file-attributes)
    nil)
   (t (let ((inhibit-file-name-handlers
             (cons #'shellfish--ios-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

(add-to-list 'file-name-handler-alist
             '("\\`/ios/clipboard\\'" . shellfish--ios-file-handler))

(provide 'shellfish)
;;; shellfish.el ends here
