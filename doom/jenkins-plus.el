;;; jenkins-plus.el --- Jenkins client for Emacs, forked from jenkins.el

;; This is a fork of jenkins.el <https://github.com/rmuslimov/jenkins.el>.
;; Original Copyright (C) 2015  Rustem Muslimov
;; Fork Copyright (C) 2026  Dylan Hardison

;; Original Author: Rustem Muslimov <r.muslimov@gmail.com>
;; Author: Dylan Hardison
;; Keywords: jenkins, convenience
;; Package-Requires: ((emacs "29.1") (dash "2.12") (json "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Changes from jenkins.el:
;; - credentials can come from auth-source (`jenkins-plus-login').
;; - the job view is a table of recent builds: number, author, duration, time.
;; - console output keeps its ANSI colors, via `ghostel-comint' when it is
;;   installed and `ansi-color' otherwise.
;; - org links of the form "jenkins:folder/job" open a job view.
;; - job views can be bookmarked.
;;
;; (setq jenkins-plus-url "<jenkins url>")
;; (setq jenkins-plus-viewname "<viewname>")

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'dash)
(require 'json)

(declare-function ghostel-comint-filter "ghostel-comint")

(defgroup jenkins-plus nil
  "Interact with a Jenkins CI server."
  :prefix "jenkins-plus-"
  :group 'tools)

(defcustom jenkins-plus-api-token nil
  "API token on user's configure page."
  :type 'string
  :group 'jenkins-plus)

(defcustom jenkins-plus-url nil
  "Jenkins URL. Example http://jenkins.company.com:80/ "
  :type 'string
  :group 'jenkins-plus)

(defcustom jenkins-plus-username nil
  "Username for Jenkins."
  :type 'string
  :group 'jenkins-plus)

(defcustom jenkins-plus-viewname nil
  "View name."
  :type 'string
  :group 'jenkins-plus)

(defcustom jenkins-plus-colwidth-id 3
  "Id column's width on main view."
  :type 'integer
  :group 'jenkins-plus)

(defcustom jenkins-plus-colwidth-name 35
  "Name column's width on main view."
  :type 'integer
  :group 'jenkins-plus)

(defcustom jenkins-plus-job-view-page-size 25
  "Number of builds the job view fetches at a time."
  :type 'integer
  :group 'jenkins-plus)

(defcustom jenkins-plus-colwidth-last-status 20
  "Status column's width on main view."
  :type 'integer
  :group 'jenkins-plus)

(defconst jenkins-plus-buffer-name
  "*jenkins-plus: status*"
  "Name of jenkins-plus buffer.")

(defun jenkins-plus-list-format ()
  "List of columns for main jenkins-plus jobs screen."
  (apply 'vector
         `(("#" ,jenkins-plus-colwidth-id f :pad-right 2 :right-align t :col-source jenkins-plus--render-indicator)
	   ("Name" ,jenkins-plus-colwidth-name t :col-source jenkins-plus--render-name)
	   ("Last success" ,jenkins-plus-colwidth-last-status f :col-source :last-success)
	   ("Last failed" ,jenkins-plus-colwidth-last-status f :col-source :last-failed))
         ))

(defun jenkins-plus--url ()
  "Return `jenkins-plus-url', guaranteed to end with a slash."
  (if (string-match-p (rx "/" string-end) jenkins-plus-url)
      jenkins-plus-url
    (concat jenkins-plus-url "/")))

(defvar-local jenkins-plus--jobs-list
  nil
  "Jobs fetched from Jenkins for the main screen.")

(defvar-local jenkins-plus-breadcrumbs
  '()
  "Job path for this buffer, innermost first.")

(defvar jenkins-plus-local-jobname)

(defun jenkins-plus-pop-and-reload ()
  "Pop the breadcrumb and reload the jenkins-plus window."
  (interactive)
  (if (= 0 (length jenkins-plus-breadcrumbs))
      (quit-window)
    (let ((breadcrumbs (cdr jenkins-plus-breadcrumbs)))
      (pop-to-buffer jenkins-plus-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only t)
      (jenkins-plus-mode)
      (setq-local jenkins-plus-breadcrumbs breadcrumbs)
      (tabulated-list-print))))

(defun jenkins-plus--render-name (item)
  "Render jobname for main jenkins-plus job ITEM screen."
  (let ((jobname (plist-get item :name))
        (progress (plist-get item :progress)))
    (if progress
        (format "%s %s"
                (propertize (format "%s%%" progress) 'font-lock-face 'warning)
                jobname)
      (format "%s" jobname))))

(defun jenkins-plus--jobs-view-url ()
  "Jenkins API URL for the jobs at the current folder level."
  (format (concat
           "%s"
           (if jenkins-plus-viewname "view/%s/" jenkins-plus-viewname "")
           (mapconcat (lambda (a) (concat "job/" a)) (reverse jenkins-plus-breadcrumbs) "/")
           "/api/"
           "json?depth=2&tree=name,jobs[name,class"
           "lastSuccessfulBuild[result,timestamp,duration,id],"
           "lastFailedBuild[result,timestamp,duration,id],"
           "lastBuild[result,executor[progress]],"
           "lastCompletedBuild[result]],"
           "class,"
           "lastSuccessfulBuild[result,timestamp,duration,id],"
           "lastFailedBuild[result,timestamp,duration,id],"
           "lastBuild[result,executor[progress]],"
           "lastCompletedBuild[result]"
           )
          (jenkins-plus--url) jenkins-plus-viewname))

(defun jenkins-plus--job-url (start end)
  "Jenkins API URL for builds START to END (0-based, exclusive) of the current job."
  (format (concat
           "%s"
           (mapconcat (lambda (a) (concat "job/" a)) (reverse jenkins-plus-breadcrumbs) "/")
           ;; `builds' stops at the newest 100; `allBuilds' has them all
           "/api/json?depth=1&tree=allBuilds"
           "[number,timestamp,duration,result,url,building,"
           "culprits[fullName]]"
           ;; {start,end} slices the builds; braces are URL-encoded
           "%%7B%d,%d%%7D")
          (jenkins-plus--url) start end))

(defun jenkins-plus--setup-variables ()
  "Ask for any connection settings that are not set yet.
Credentials are looked up with auth-source before prompting."
  (unless jenkins-plus-url
    (setq jenkins-plus-url (read-from-minibuffer "Jenkins URL: ")))
  (unless (and jenkins-plus-username jenkins-plus-api-token)
    (ignore-errors (jenkins-plus-login)))
  (unless jenkins-plus-username
    (setq jenkins-plus-username (read-from-minibuffer "Jenkins username: ")))
  (unless jenkins-plus-api-token
    (setq jenkins-plus-api-token (read-from-minibuffer "Jenkins API Token: "))))

;; models

(defun jenkins-plus--make-job (class name result progress last-success last-failed)
  "Define regular jenkins-plus job here."
  (list :name name
        :result result
        :progress progress
        :last-success last-success
        :last-failed last-failed
        :class class))

(defun jenkins-plus--face-for-result (result)
  "Simple function returning proper 'face for jenkins-plus RESULT."
  (let ((facemap (list '("SUCCESS" . 'success)
                       '("FAILURE" . 'error)
                       '("ABORTED" . 'warning))))
    (cdr (assoc result facemap))))

(defun jenkins-plus--render-indicator (job)
  "Special indicator for each JOB on main jenkins-plus window."
  (propertize
   "●" 'font-lock-face
   (jenkins-plus--face-for-result
    (plist-get job :result))))

(defun jenkins-plus--main-list-entries ()
  "Build `tabulated-list-entries' from `jenkins-plus--jobs-list'."
  (--map
   (list
    (plist-get it :name)
    (apply 'vector
           (-map
            (lambda (column)
              (let* ((args (nthcdr 3 column))
                     (col-source (plist-get args :col-source)))
                (if (functionp col-source)
                    (funcall col-source it)
                  (plist-get it col-source))))
            (jenkins-plus-list-format))))
   (mapcar 'cdr jenkins-plus--jobs-list)))

;;; actions

(defun jenkins-plus-enter-job (&optional jobindex)
  "Open the job JOBINDEX, or the job at point."
  (interactive)
  (when-let ((jobindex (or jobindex (tabulated-list-get-id))))
    (jenkins-plus-job-view jobindex (cons jobindex jenkins-plus-breadcrumbs))))

(defun jenkins-plus--open-path (parents jobname)
  "Show the main buffer at PARENTS (innermost-first), then enter JOBNAME."
  (pop-to-buffer jenkins-plus-buffer-name)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq buffer-read-only t)
  (jenkins-plus-mode)
  (setq-local jenkins-plus-breadcrumbs parents)
  ;; also populates `jenkins-plus--jobs-list', needed for the class lookup
  (tabulated-list-print)
  (jenkins-plus-enter-job jobname))

(defun jenkins-plus--parse-time-from (time-since timeitems)
  (let* ((timeitem (car timeitems))
         (extracted-time (mod time-since (cdr timeitem)))
         (rest-time (/ (- time-since extracted-time) (cdr timeitem)))
         )
    (if (cdr timeitems)
        (apply 'list
               (list extracted-time (car timeitem))
               (jenkins-plus--parse-time-from rest-time (cdr timeitems)))
      (list (list time-since (car timeitem)))
      )))

(defun jenkins-plus--time-since-to-text (timestamp)
  "Describe how long ago TIMESTAMP was, or show its date if it was more than a day ago."
  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins-plus--parse-time-from seconds-since timeitems))
         )
    (if (>= seconds-since 86400)
        (format-time-string "%Y-%m-%d %H:%M" timestamp)
      (let ((since (mapconcat
                    (lambda (values) (apply 'format "%d%s" values))
                    (-take 3 (reverse (--filter (not (= (car it) 0)) time-pairs)))
                    ":")))
        (if (string-empty-p since) "just now" (concat since " ago"))))))

(defun jenkins-plus--duration-to-text (duration-ms)
  "Format DURATION-MS (as returned by Jenkins, in milliseconds) as a short string."
  (if (and duration-ms (> duration-ms 0))
      (let ((time-pairs (jenkins-plus--parse-time-from
                         (/ duration-ms 1000)
                         '(("s" . 60) ("m" . 60) ("h" . 24) ("d" . 1)))))
        (mapconcat
         (lambda (values) (apply 'format "%d%s" values))
         (-take 2 (reverse (--filter (not (= (car it) 0)) time-pairs)))
         ""))
    ""))

(defun jenkins-plus--main-list-refresh ()
  "Fetch jobs from Jenkins and return them as table entries."
  (jenkins-plus--fetch-jobs-list)
  (jenkins-plus--main-list-entries))

(defun jenkins-plus--auth-headers ()
  "HTTP headers for authenticating to Jenkins."
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Authorization" .
     ,(concat
       "Basic "
       (base64-encode-string
        (concat jenkins-plus-username ":" jenkins-plus-api-token) t)))))

(defun jenkins-plus--get-json (url)
  "Fetch URL from Jenkins and parse the response as JSON."
  (let ((url-request-extra-headers (jenkins-plus--auth-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (prog1 (json-read-from-string
              (decode-coding-string
               (buffer-substring-no-properties (point) (point-max)) 'utf-8))
        (kill-buffer)))))

(defun jenkins-plus--extract-time-of-build (x buildname)
  "Helper defun to render timestamps."
  (let ((val (cdr (assoc 'timestamp (assoc buildname x)))))
    (if val (jenkins-plus--time-since-to-text (/ val 1000)) "")))

(defun jenkins-plus--fetch-jobs-list ()
  "Fetch the jobs at the current folder level from Jenkins.
Each job includes its `_class', used to tell folders from other jobs."
  (setq
   jenkins-plus--jobs-list
   (let* ((jobs-url (jenkins-plus--jobs-view-url))
          (raw-data (jenkins-plus--get-json jobs-url))
          (jobs (cdr (assoc 'jobs raw-data))))
     (--map
      (apply 'list (cdr (assoc 'name it))
             (jenkins-plus--make-job
              (cdr (assoc '_class it))
              (cdr (assoc 'name it))
              (cdr (assoc 'result (assoc 'lastCompletedBuild it)))
              (cdr (assoc 'progress (assoc 'executor (assoc 'lastBuild it))))
              (jenkins-plus--extract-time-of-build it 'lastSuccessfulBuild)
              (jenkins-plus--extract-time-of-build it 'lastFailedBuild)))
      jobs))))

(defvar jenkins-plus-job-view-columns
  [("#" 3 nil)
   ("Build" 8 nil)
   ("Author" 20 t)
   ("Duration" 10 nil)
   ("Time" 16 nil)]
  "Column format for the jenkins-plus job-view's build table.")

;; jenkins-plus--job-view-builds is an alist of (NUMBER . PLIST), newest first
(defvar-local jenkins-plus--job-view-builds nil
  "Builds loaded into this job-view buffer, newest first.")

(defun jenkins-plus--job-view-entries ()
  "Build `tabulated-list-entries' from `jenkins-plus--job-view-builds'."
  (--map
   (let ((build (cdr it)))
     (list
      (format "%s" (car it))
      (vector
       (propertize "●" 'font-lock-face
                   (jenkins-plus--face-for-result (plist-get build :result)))
       (if-let ((url (plist-get build :url)))
           (buttonize (format "%s" (car it)) #'browse-url url)
         (format "%s" (car it)))
       (or (plist-get build :author) "---")
       (jenkins-plus--duration-to-text (plist-get build :duration))
       (or (plist-get build :timestring) ""))))
   jenkins-plus--job-view-builds))

(defun jenkins-plus--job-view-update-header-line ()
  "Show the result of the newest finished build in the mode-name."
  (let ((result (plist-get (cdr (--first (eq (plist-get (cdr it) :building) :json-false)
                                         jenkins-plus--job-view-builds))
                           :result)))
    (setq mode-name
          (concat "Jenkins Job: "
                  (propertize (or result "?")
                              'face (jenkins-plus--face-for-result result))))))

(defun jenkins-plus--job-view-refresh ()
  "Refetch the builds loaded in this job view and redraw the table.
Refetches as many builds as are loaded, and at least one batch."
  (setq jenkins-plus--job-view-builds
        (jenkins-plus--fetch-builds
         0 (max (length jenkins-plus--job-view-builds)
                jenkins-plus-job-view-page-size)))
  (jenkins-plus--job-view-update-header-line)
  (tabulated-list-print t))

(defun jenkins-plus--fetch-builds (start end)
  "Fetch builds START to END (0-based, exclusive) of the current job, newest first."
  (cl-labels ((retrieve (attr item)
                (cdr (assoc attr item)))
              (convert-item (item)
                (list
                 (retrieve 'number item)
                 :author (let ((culprits (cdr (assoc 'culprits item))))
                           (if (> (length culprits) 0)
                               (cdar (aref culprits 0)) "---"))
                 :url (retrieve 'url item)
                 :timestring (jenkins-plus--time-since-to-text (/ (retrieve 'timestamp item) 1000))
                 :duration (retrieve 'duration item)
                 :building (retrieve 'building item)
                 :result (retrieve 'result item))))
    (-map #'convert-item
          (alist-get 'allBuilds
                     (jenkins-plus--get-json (jenkins-plus--job-url start end))))))

;; helpers
(defun jenkins-plus-visit-jenkins-web-page ()
  "Open the Jenkins web UI."
  (interactive)
  (browse-url (jenkins-plus--url)))

(defun jenkins-plus-visit-job (jobname)
  "Open job's webpage using JOBNAME."
  (interactive)
  (browse-url (format "%s%s/" (jenkins-plus--url)
                      (mapconcat (lambda (a) (concat "job/" a)) (reverse jenkins-plus-breadcrumbs) "/")
                      )))

(defun jenkins-plus-get-console-output (jobname build)
  "Show the console output of JOBNAME's BUILD, with ANSI colors rendered."
  (let* ((url-request-extra-headers (jenkins-plus--auth-headers))
         (console-buffer (get-buffer-create
                          (format "*jenkins-plus-console-%s-%s*" jobname build)))
         (url (format "%s%s/%s/consoleText" (jenkins-plus--url)
                      (mapconcat (lambda (a) (concat "job/" a))
                                 (reverse jenkins-plus-breadcrumbs) "/")
                      build))
         (raw (with-current-buffer (url-retrieve-synchronously url)
                (goto-char (point-min))
                (re-search-forward "^\r?$")        ; end of HTTP headers
                (prog1 (decode-coding-string
                        (buffer-substring-no-properties (1+ (point)) (point-max))
                        'utf-8)
                  (kill-buffer)))))
    (with-current-buffer console-buffer
      (read-only-mode -1)
      (erase-buffer)
      ;; font-lock is turned off below, so neither path may rely on
      ;; 'font-lock-face
      (if (require 'ghostel-comint nil t)
          (insert (ghostel-comint-filter raw))
        (insert raw)
        (ansi-color-apply-on-region (point-min) (point-max)))
      (comint-carriage-motion (point-min) (point-max))
      (goto-char (point-min)))
    (pop-to-buffer console-buffer)
    (jenkins-plus-console-output-mode)
    (font-lock-mode -1)))

(defun jenkins-plus--call-on-job-at-point (fn)
  "Call FN with the job at point added to `jenkins-plus-breadcrumbs'."
  (if-let ((jobname (tabulated-list-get-id)))
      (let ((jenkins-plus-breadcrumbs (cons jobname jenkins-plus-breadcrumbs)))
        (funcall fn jobname))
    (user-error "No job on this line")))

(defun jenkins-plus--visit-job-from-main-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-plus--call-on-job-at-point #'jenkins-plus-visit-job))

(defun jenkins-plus--visit-job-from-job-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-plus-visit-job jenkins-plus-local-jobname))

(defun jenkins-plus--console-output-from-job-screen ()
  "Show the console output for the build at point."
  (interactive)
  (if-let ((build (tabulated-list-get-id)))
      (jenkins-plus-get-console-output jenkins-plus-local-jobname build)
    (user-error "No build on this line")))

;; emacs major mode funcs and variables
(defvar-keymap jenkins-plus-mode-map
  :doc "Keymap for `jenkins-plus-mode'."
  "RET" #'jenkins-plus-enter-job
  "b"   #'jenkins-plus--build-from-main-screen
  "r"   #'jenkins-plus--rebuild-from-main-screen
  "v"   #'jenkins-plus--visit-job-from-main-screen
  "q"   #'jenkins-plus-pop-and-reload)

(defvar-keymap jenkins-plus-job-view-mode-map
  :doc "Keymap for `jenkins-plus-job-view-mode'."
  "RET" #'jenkins-plus--console-output-from-job-screen
  "g"   #'jenkins-plus--refresh-from-job-screen
  "l"   #'jenkins-plus-latest-build-console
  "b"   #'jenkins-plus--build-from-job-screen
  "r"   #'jenkins-plus--rebuild-from-job-screen
  "v"   #'jenkins-plus--visit-job-from-job-screen
  "+"   #'jenkins-plus-job-view-load-more)

(define-derived-mode jenkins-plus-mode tabulated-list-mode "Jenkins"
  "Major mode for the Jenkins job list."
  (setq truncate-lines t)
  (hl-line-mode 1)
  (setq tabulated-list-format (jenkins-plus-list-format))
  (setq tabulated-list-entries 'jenkins-plus--main-list-refresh)
  (tabulated-list-init-header))

(define-derived-mode jenkins-plus-job-view-mode tabulated-list-mode "Jenkins Job"
  "Tabulated-list mode showing the build history of a jenkins-plus job."
  (setq tabulated-list-format jenkins-plus-job-view-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'jenkins-plus--job-view-entries)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-derived-mode jenkins-plus-console-output-mode special-mode "Jenkins Log"
  "Major mode for Jenkins console output."
  ;; remove ^M from the output
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun jenkins-plus-job-view (jobname breadcrumbs)
  "Open JOBNAME.  BREADCRUMBS is its path, innermost first."
  (let* ((details-buffer-name (format "*jenkins-plus: %s details*" jobname))
         (job (cdr (assoc jobname jenkins-plus--jobs-list)))
         (class (plist-get job :class)))
    (if (member class '("jenkins.branch.OrganizationFolder"
                        "org.jenkinsci.plugins.workflow.multibranch.WorkflowMultiBranchProject"
                        "com.cloudbees.hudson.plugins.folder.Folder"))
        (progn
          (jenkins-plus-mode)
          (setq-local jenkins-plus-breadcrumbs breadcrumbs)
          (tabulated-list-print))
      (switch-to-buffer details-buffer-name)
      (jenkins-plus-job-view-mode)
      (setq-local jenkins-plus-breadcrumbs breadcrumbs)
      (setq-local jenkins-plus-local-jobname jobname)
      (jenkins-plus--job-view-refresh)
      (goto-char (point-min)))))

(defun jenkins-plus--call-build (jobname)
  "Start a build of JOBNAME."
  (let ((url-request-extra-headers (jenkins-plus--auth-headers))
        (url-request-method "POST")
        (build-url (format "%s%s/build" (jenkins-plus--url)
                           (mapconcat (lambda (a) (concat "job/" a)) (reverse jenkins-plus-breadcrumbs) "/"))))
    (when (y-or-n-p (format "Ready to start %s?" jobname))
      (with-current-buffer (url-retrieve-synchronously build-url)
        (message (format "Building %s job started!" jobname))))))

(defun jenkins-plus--call-rebuild (jobname)
  "Rebuild JOBNAME."
  (let ((url-request-extra-headers (jenkins-plus--auth-headers))
        (url-request-method "GET")
        (build-url (format "%s%s/build" (jenkins-plus--url)
                           (mapconcat (lambda (a) (concat "job/" a)) (reverse jenkins-plus-breadcrumbs) "/"))))
    (when (y-or-n-p (format "Ready to rebuild %s?" jobname))
      (with-current-buffer (url-retrieve-synchronously build-url)
        (message (format "Building %s job started!" jobname))))))

(defun jenkins-plus--build-from-main-screen ()
  "Build job from main screen."
  (interactive)
  (jenkins-plus--call-on-job-at-point #'jenkins-plus--call-build))

(defun jenkins-plus--build-from-job-screen ()
  "Call building job from job details in jenkins-plus."
  (interactive)
  (jenkins-plus--call-build jenkins-plus-local-jobname))

(defun jenkins-plus--rebuild-from-main-screen ()
  "rebuild job from main screen."
  (interactive)
  (jenkins-plus--call-on-job-at-point #'jenkins-plus--call-rebuild))

(defun jenkins-plus--rebuild-from-job-screen ()
  "Call rebuilding job from job details in jenkins-plus."
  (interactive)
  (jenkins-plus--call-rebuild jenkins-plus-local-jobname))

(defun jenkins-plus--refresh-from-job-screen ()
  "Refetch build details for the current job and redraw the table."
  (interactive)
  (jenkins-plus--job-view-refresh))

(defun jenkins-plus-job-view-load-more ()
  "Add the next `jenkins-plus-job-view-page-size' older builds to the table."
  (interactive)
  (let* ((loaded (length jenkins-plus--job-view-builds))
         (more (jenkins-plus--fetch-builds
                loaded (+ loaded jenkins-plus-job-view-page-size))))
    (unless more
      (user-error "No older builds"))
    (setq jenkins-plus--job-view-builds (append jenkins-plus--job-view-builds more))
    (tabulated-list-print t)
    (message "Showing %d builds" (length jenkins-plus--job-view-builds))))

(defun jenkins-plus-latest-build-console ()
  "Open console output for the most recent build of the current job."
  (interactive)
  (if-let ((build (caar jenkins-plus--job-view-builds)))
      (jenkins-plus-get-console-output jenkins-plus-local-jobname (format "%s" build))
    (user-error "No builds listed")))

(defun jenkins-plus-login ()
  "Read Jenkins credentials from auth-source."
  (interactive)
  (let ((host (url-host (url-generic-parse-url jenkins-plus-url))))
    (if-let* ((entry (car (auth-source-search :host host)))
              (user (plist-get entry :user))
              (secret (plist-get entry :secret))
              (password (if (functionp secret) (funcall secret) secret)))
        (setq jenkins-plus-username user
              jenkins-plus-api-token password)
      (user-error "No authinfo entry found for %s" host))))

;;;###autoload
(defun jenkins-plus ()
  "Initialize jenkins-plus buffer."
  (interactive)
  (jenkins-plus--setup-variables)
  (pop-to-buffer jenkins-plus-buffer-name)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq buffer-read-only t)
  (jenkins-plus-mode)
  (setq-local jenkins-plus-breadcrumbs nil)
  (tabulated-list-print))

;;;###autoload
(with-eval-after-load 'org
  (defun jenkins-plus-org-follow (path _)
    "Open jenkins-plus job from PATH like \"folder/subfolder/jobname\"."
    (require 'jenkins-plus)
    (jenkins-plus--setup-variables)
    (let* ((parts (split-string path "/" t))
           (jobname (car (last parts)))
           (parents (butlast parts)))
      ;; breadcrumbs are stored innermost-first (it's a push-down list)
      (jenkins-plus--open-path (reverse parents) jobname)))

  (defun jenkins-plus-org-store ()
    "Store a link to the current jenkins-plus job view."
    (when (derived-mode-p 'jenkins-plus-job-view-mode)
      (let ((path (string-join (reverse jenkins-plus-breadcrumbs) "/")))
        (org-link-store-props
         :type "jenkins"
         :link (concat "jenkins:" path)
         :description (format "Jenkins: %s" jenkins-plus-local-jobname)))))

  (org-link-set-parameters
   "jenkins"
   :follow #'jenkins-plus-org-follow
   :store #'jenkins-plus-org-store))

(defun jenkins-plus--bookmark-make-record ()
  "Create a bookmark record for the current jenkins-plus job view."
  `(,(format "jenkins:%s" jenkins-plus-local-jobname)
    (jenkins-jobname . ,jenkins-plus-local-jobname)
    (jenkins-breadcrumbs . ,jenkins-plus-breadcrumbs)
    (handler . jenkins-bookmark-handler)))

;;;###autoload
(defun jenkins-bookmark-handler (bookmark)
  "Restore a jenkins-plus job view from BOOKMARK."
  (require 'jenkins-plus)
  (jenkins-plus--setup-variables)
  (let ((jobname (bookmark-prop-get bookmark 'jenkins-jobname)))
    ;; The saved breadcrumbs end with the job itself.  Drop it to get
    ;; the folder.
    (jenkins-plus--open-path
     (cdr (bookmark-prop-get bookmark 'jenkins-breadcrumbs))
     jobname)))

(add-hook 'jenkins-plus-job-view-mode-hook
          (lambda ()
            (setq-local bookmark-make-record-function
                        #'jenkins-plus--bookmark-make-record)))

(provide 'jenkins-plus)
;;; jenkins-plus.el ends here
