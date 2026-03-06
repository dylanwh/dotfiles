;;; org-links.el --- Org capture links with URL metadata fetching -*- lexical-binding: t; -*-

;; Author: Dylan Hardison <dylan@hardison.net>
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Provides `org-links-capture-template' for use in `org-capture-templates'.
;; Fetches Open Graph and HTML metadata (title, description) from URLs
;; and pre-fills the capture prompts.
;;
;; Title fixups per domain are configurable via `org-links-title-fixup-functions'.

;;; Code:

(require 'url)
(require 'dom)
(require 'cl-lib)

(defgroup org-links nil
  "Org capture links with URL metadata fetching."
  :group 'org
  :prefix "org-links-")

(defcustom org-links-title-fixup-functions
  '(("github\\.com" . org-links-fixup-github-title))
  "Alist of (DOMAIN-REGEXP . FUNCTION) for fixing up page titles.
Each function receives a title string and should return a cleaned-up title."
  :type '(alist :key-type regexp :value-type function)
  :group 'org-links)

(defcustom org-links-request-timeout 10
  "Timeout in seconds for fetching URL metadata."
  :type 'integer
  :group 'org-links)

(defun org-links-fixup-github-title (title)
  "Clean GitHub's \"GitHub - user/repo: description\" title format."
  (if (string-match "\\`GitHub - \\([^:]+\\)" title)
      (match-string 1 title)
    title))

(defun org-links--fixup-title (url title)
  "Apply any matching fixup function from `org-links-title-fixup-functions' to TITLE."
  (if title
      (let ((host (url-host (url-generic-parse-url url))))
        (cl-loop for (pattern . fn) in org-links-title-fixup-functions
                 when (string-match-p pattern host)
                 return (funcall fn title)
                 finally return title))
    title))

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
    (setq title (org-links--fixup-title url title))
    `((:title . ,title) (:description . ,description))))

(defun org-links-capture-template ()
  "Return an org-capture template string for a link with auto-fetched metadata.
Prompts for URL, fetches title and description, then returns a template
string safe for org-capture (literal %% escaped)."
  (let* ((url (read-string "URL: "))
         (metadata (org-links-fetch-metadata url))
         (fetched-title (alist-get :title metadata))
         (fetched-desc (alist-get :description metadata))
         (title (read-string "Title: " fetched-title))
         (desc (if fetched-desc
                   (read-string "Description: " fetched-desc)
                 (read-string "Description: ")))
         ;; Escape % so org-capture does not interpret them
         (url (replace-regexp-in-string "%" "%%" url))
         (title (replace-regexp-in-string "%" "%%" title))
         (desc (when desc (replace-regexp-in-string "%" "%%" desc))))
    (concat "* [[" url "][" title "]]\n"
            "  :PROPERTIES:\n"
            "  :CREATED: %U\n"
            "  :END:\n"
            (when (and desc (not (string-empty-p desc)))
              (concat desc "\n")))))

(provide 'org-links)
;;; org-links.el ends here
