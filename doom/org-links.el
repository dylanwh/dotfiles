;;; org-links.el --- Org capture links -*- lexical-binding: t; -*-

;; Author: Dylan Hardison <dylan@hardison.net>
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Provides `org-links-capture-template' for use in `org-capture-templates'.
;; When invoked via org-protocol, uses URL/title from the protocol.
;; When invoked interactively, prompts for URL and fetches metadata.
;;
;; Fixups per domain are configurable via `org-links-fixup-functions'.

;;; Code:

(require 'url)
(require 'dom)
(require 'cl-lib)
(require 'org)

(defgroup org-links nil
  "Org capture links."
  :group 'org
  :prefix "org-links-")

(defcustom org-links-fixup-functions
  '(("github\\.com" . org-links--fixup-github))
  "Alist of (DOMAIN-REGEXP . FUNCTION) for fixing up link metadata.
Each function receives a plist (:url :title :body) and should return
a plist with the same keys, with any values cleaned up."
  :type '(alist :key-type regexp :value-type function)
  :group 'org-links)

(defcustom org-links-request-timeout 10
  "Timeout in seconds for fetching URL metadata."
  :type 'integer
  :group 'org-links)

(defun org-links--fixup-github (plist)
  "Clean GitHub metadata in PLIST.
Strips description suffix from title and trailing \" - user/repo\" from body."
  (let ((title (plist-get plist :title))
        (body (plist-get plist :body)))
    (when title
      (setq title
            (cond
             ((string-match "\\`GitHub - \\([^:]+\\)" title)
              (string-trim (match-string 1 title)))
             ((string-match "\\`\\([^/]+/[^:]+\\):" title)
              (string-trim (match-string 1 title)))
             (t title)))
      (setq plist (plist-put plist :title title)))
    (when body
      (setq plist (plist-put plist :body
                             (replace-regexp-in-string
                              " - [^/]+/[^ ]+\\'" "" body))))
    plist))

(defun org-links--fixup (plist)
  "Apply domain-matched fixup functions to PLIST (:url :title :body)."
  (let ((host (url-host (url-generic-parse-url (plist-get plist :url)))))
    (cl-loop for (pattern . fn) in org-links-fixup-functions
             when (string-match-p pattern host)
             return (funcall fn plist)
             finally return plist)))

(defun org-links--dom-meta-content (dom property &optional attr)
  "Find a <meta> tag in DOM where ATTR equals PROPERTY and return its content.
ATTR defaults to \"property\" (for Open Graph tags)."
  (let ((attr (or attr "property")))
    (cl-loop for meta in (dom-by-tag dom 'meta)
             when (equal (dom-attr meta (intern attr)) property)
             return (dom-attr meta 'content))))

(defun org-links-fetch-metadata (url)
  "Fetch URL and return an alist with :title and :description from HTML metadata.
Prefers Open Graph tags, falls back to <title> and <meta name=\"description\">."
  (let* ((url-request-extra-headers
          '(("Accept" . "text/html,application/xhtml+xml")))
         (buf (ignore-errors
                (url-retrieve-synchronously url t nil org-links-request-timeout)))
         title description)
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (let ((dom (libxml-parse-html-region (point) (point-max))))
                (setq title
                      (or (org-links--dom-meta-content dom "og:title")
                          (let ((el (dom-by-tag dom 'title)))
                            (when el (string-trim (dom-texts (car el)))))))
                (setq description
                      (or (org-links--dom-meta-content dom "og:description")
                          (org-links--dom-meta-content dom "description" "name"))))))
        (kill-buffer buf)))
    `((:title . ,title) (:body . ,description))))

(defun org-links--notify (message)
  "Send an OSC 777 notification with MESSAGE."
  (when-let* ((tty (terminal-name (frame-terminal))))
    (send-string-to-terminal
     (format "\e]777;notify;emacs;%s\e\\" message)
     (frame-terminal))))

(defun org-links--escape-%% (str)
  "Escape % as %% in STR for org-capture templates."
  (when str (replace-regexp-in-string "%" "%%" str)))

(defun org-links-capture-template ()
  "Return an `org-capture' template string for a link with auto-fetched metadata.
Prompts for URL, fetches title and description, then returns a template
string safe for org-capture (literal %% escaped)."
  (let* ((url (read-string "URL: "))
         (metadata (org-links-fetch-metadata url))
         (fixed (org-links--fixup
                 (list :url url
                       :title (alist-get :title metadata)
                       :body (alist-get :body metadata))))
         (title (read-string "Title: " (plist-get fixed :title)))
         (desc (read-string "Description: " (plist-get fixed :body)))
         (url (org-links--escape-%% url))
         (title (org-links--escape-%% title))
         (desc (org-links--escape-%% desc)))
    (concat "* [[" url "][" title "]]\n"
            "  :PROPERTIES:\n"
            "  :CREATED: %U\n"
            "  :END:\n"
            (if (and desc (not (string-empty-p desc)))
                (concat desc "\n%?")
              "%?"))))

(defun org-links-protocol-capture-template ()
  "Return an `org-capture' template string for a link from org-protocol.
Fills in URL/title from `org-store-link-plist'."
  (org-links--notify "link captured")
  (let* ((url (plist-get org-store-link-plist :link))
         (title (or (plist-get org-store-link-plist :description) url))
         (body (plist-get org-store-link-plist :initial))
         (fixed (org-links--fixup (list :url url :title title :body body)))
         (url (org-links--escape-%% (plist-get fixed :url)))
         (title (org-links--escape-%% (plist-get fixed :title)))
         (body (org-links--escape-%% (plist-get fixed :body))))
    (concat "* [[" url "][" title "]]\n"
            "  :PROPERTIES:\n"
            "  :CREATED: %U\n"
            "  :END:\n"
            (if (and body (not (string-empty-p body)))
                (concat body "\n%?")
              "%?"))))

(defun org-links-heading-url ()
  "Return the first bracketed link URL found in the current heading's title, or nil."
  (save-excursion
    (org-back-to-heading t)
    (looking-at org-complex-heading-regexp)
    (let ((title (match-string-no-properties 4)))
      (with-temp-buffer
        (insert title)
        (goto-char (point-min))
        (when (re-search-forward org-link-bracket-re nil t)
          (match-string-no-properties 1))))))


(defun org-links--wayback-url (url)
  "Return the closest Wayback Machine snapshot URL for URL, or nil if unavailable."
  (let* ((api-url (concat "https://archive.org/wayback/available?url="
                          (url-hexify-string url)))
         (buf (ignore-errors
                (url-retrieve-synchronously api-url t nil org-links-request-timeout))))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (data (ignore-errors (json-read)))
                     (snapshots (plist-get data :archived_snapshots))
                     (closest (plist-get snapshots :closest)))
                (when (eq (plist-get closest :available) t)
                  (plist-get closest :url)))))
        (kill-buffer buf)))))

(defun org-links-wayback-replace-link ()
  "Replace the link URL in the current heading title with its Wayback Machine URL.
Stores the original URL in the ORIGINAL_URL heading property."
  (interactive)
  (let ((url (org-links-heading-url)))
    (unless url
      (user-error "No link found in heading title"))
    (message "Looking up Wayback URL for %s..." url)
    (let ((wayback (org-links--wayback-url url)))
      (unless wayback
        (user-error "No Wayback Machine snapshot found for %s" url))
      (org-set-property "ORIGINAL_URL" url)
      (save-excursion
        (org-back-to-heading t)
        (let ((bound (line-end-position)))
          (when (re-search-forward (regexp-quote url) bound t)
            (replace-match wayback t t))))
      (message "Replaced with Wayback URL: %s" wayback))))

(provide 'org-links)
;;; org-links.el ends here
