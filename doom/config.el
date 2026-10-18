;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dylan Hardison"
      user-mail-address "dylan@hardison.net")

;; for some reason emacs doesn't understand the TZ env I set.
(set-time-zone-rule "PST8PDT")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :style "Light" :size 14.0))
(setq doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :style "Light" :size 28.0))
(setq nerd-icons-font-family "SauceCodePro Nerd Font Mono")

(with-eval-after-load 'vterm
  (remove-hook 'vterm-mode-hook #'mode-line-invisible-mode))
(with-eval-after-load 'ghostel
  (remove-hook 'ghostel-mode-hook #'mode-line-invisible-mode))
(with-eval-after-load 'eshell
  (remove-hook 'eshell-mode-hook #'mode-line-invisible-mode))
(with-eval-after-load 'shell
  (remove-hook 'shell-mode-hook #'mode-line-invisible-mode))
(with-eval-after-load 'term
  (remove-hook 'term-mode-hook #'mode-line-invisible-mode))

(with-eval-after-load 'popup
  (set-popup-rule! "^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
    :vslot -5 :size 0.20 :select t :modeline t :quit nil :ttl nil))

(add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)
(add-hook 'doom-first-buffer-hook #'ghostel-compile-global-mode)
(add-hook 'doom-first-buffer-hook #'ghostel-comint-global-mode)

(setq +doom-dashboard-ascii-banner-fn (lambda () ))

(setq doom-modeline-unicode-fallback nil)
(setq xterm-extra-capabilities '(reportBackground))
(xterm-mouse-mode t)

;; I like to have plenty of time to read a message
(setq minibuffer-message-timeout 60)

(setq initial-buffer-choice (lambda () (doom/switch-to-project-scratch-buffer)))
(setq switch-window-shortcut-appearance 'asciiart)

;; Org-mode configuration
;; 
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(load! "my-links.el")

(with-eval-after-load 'org
  ;; Log timestamp when a task is marked DONE
  (setq org-log-done 'time)
  (setq org-priority-lowest ?F)
  (setq org-babel-tangle-use-relative-file-links nil)
  (setq org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)
          (?D . font-lock-keyword-face)
          (?E . font-lock-comment-face)
          (?F . shadow))))

(global-orglink-mode t)
(add-to-list 'orglink-activate-in-modes 'ssh-config-mode)

(with-eval-after-load 'org-capture
  (require 'org-protocol)
  ;; Remove before adding to avoid duplicates on re-eval.
  (setq org-capture-templates
        (cl-remove "l" org-capture-templates :key #'car :test #'string=))
  (setq org-capture-templates
        (cl-remove "L" org-capture-templates :key #'car :test #'string=))
  (add-to-list 'org-capture-templates
               '("l" "Link" entry
                 (file+headline "~/org/links.org" "Inbox")
                 (function my-links-capture-template)) t)
  (add-to-list 'org-capture-templates
               '("L" "Link (protocol)" entry
                 (file+headline "~/org/links.org" "Inbox")
                 (function my-links-protocol-capture-template)
                 :immediate-finish t) t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(load! "ssh-auth-sock.el")
(load! "ssh-hosts.el")
(load! "shellfish.el")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "perltidy.el")

(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode ()
  (cperl-set-style "Whitesmith"))

(with-eval-after-load 'cperl-mode
  (define-key cperl-mode-map "{" nil)
  (setq cperl-highlight-variables-indiscriminately t)

  (add-hook 'cperl-mode-hook #'my-cperl-mode))

;; emacs doesn't typically consider _ to be a word character, which is annoying
;; in several languages. Modify those modes syntax entries to consider _ a word
;; character
(let ((wordy-modes '(python-mode-hook cperl-mode-hook ruby-mode-hook
                     js2-mode-hook c-mode-hook rust-mode-hook lua-mode-hook))
      (hook #'(lambda () (modify-syntax-entry ?_ "w"))))
  (dolist (mode wordy-modes)
    (add-hook mode hook)))

(add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
(setq-default eglot-workspace-configuration
              '(:rust-analyzer (:inlayHints (:closureReturnTypeHints (:enable "always")
                                             :parameterHints (:enable t)))))

(setq eglot-code-action-indicator ""
      eglot-auto-display-help-buffer t
      eglot-x-graph-type 'boxart)

(with-eval-after-load 'eldoc
  (setq eldoc-echo-area-prefer-doc-buffer nil
        eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy 'eldoc-documentation-enthusiast))

(use-package eglot-x
  :after eglot
  :config

  ;; Enhanced reference finding (supports ccls, rust-analyzer extended methods)
  (map! :leader
        :desc "Find references (extended)" "c ." #'eglot-x-find-refs)

  ;; Rust-analyzer keybindings
  (map! :map rustic-mode-map
        :localleader
        (:prefix ("r" . "rust")
         :desc "Expand macro"             "e" #'eglot-x-expand-macro
         :desc "View memory layout"       "m" #'eglot-x-view-recursive-memory-layout
         :desc "Related tests"            "t" #'eglot-x-ask-related-tests
         :desc "Reload workspace"         "r" #'eglot-x-reload-workspace
         :desc "View crate graph"         "g" #'eglot-x-view-crate-graph
         :desc "Find crate"               "f" #'eglot-x-find-crate
         :desc "Analyzer status"          "s" #'eglot-x-analyzer-status
         :desc "View syntax tree"         "S" #'eglot-x-view-syntax-tree))

  (eglot-x-setup))

(setq confirm-kill-emacs #'yes-or-no-p)

(map! :leader :desc "GitHub link" :n "g h l" #'git-link)
(map! :leader :desc "Magit worktree" :n "g %" #'magit-worktree)
(map! :leader :desc "Magit push" :n "g P" #'magit-push-current-to-upstream)

(use-package shelldon
  :defer t
  :commands (shelldon shelldon-async-command shelldon-output-history))

(defun project-shelldon-async-command ()
  "Run `shelldon-async-command' in the current project's root directory."
  (declare (interactive-only shelldon-async-command))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shelldon-async-command)))

(defun project-shelldon ()
  "Run `shell-command' in the current project's root directory."
  (declare (interactive-only shelldon))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shelldon)))

(setq display-buffer-alist
      (cons '("*shelldon:"
              (display-buffer-in-side-window display-buffer-reuse-window display-buffer-in-previous-window)
              (side . right)
              (slot . 3)
              (reusable-frames . visible)
              (window-width . 0.34)
              (inhibit-switch-frame . t)
              (window-parameters (modeline . t)))
            (cl-remove-if (lambda (entry)
                            (and (stringp (car entry))
                                 (string-match-p "shelldon" (car entry))))
                          display-buffer-alist)))

(defun my/shelldon-dismiss ()
  "Quit all shelldon windows, restoring previous window state."
  (interactive)
  (dolist (win (window-list))
    (when (string-match-p "\\*shelldon:" (buffer-name (window-buffer win)))
      (quit-window nil win))))

(map! :ni "M-&" #'project-shelldon-async-command)
(map! :ni "M-!" #'project-shelldon)

(defun my/configure-opener ()
  "Configure SSH for opener."
  (interactive)
  (shelldon-async-command "echo \"StreamLocalBindUnlink yes\" | sudo tee /etc/ssh/sshd_config.d/opener.conf"))

(defun my/smart-rebuild ()
  "Run smart-rebuild."
  (interactive)
  (shelldon-async-command "smart-rebuild"))

(defun my/imapfilter ()
  "Run imapfilter"
  (interactive)
  (shelldon-async-command "imapfilter"))

(defun my/doom-sync ()
  "Run doom sync with AOT compilation and GC."
  (interactive)
  (shelldon-async-command "smart-doom-sync"))

(defun my/ssh-add ()
  "Add SSH keys to agent."
  (interactive)
  (let ((key-files '("~/.ssh/id_ed25519" "~/.ssh/id_rsa")))
    (cond
     ((eq system-type 'darwin)
      (call-process "ssh-add" nil nil nil "-q" "--apple-load-keychain")
      (dolist (file key-files)
        (when (file-exists-p (expand-file-name file))
          (call-process "ssh-add" nil nil nil "--apple-use-keychain" (expand-file-name file)))))
     (t
      (unless (zerop (call-process "ssh-add" nil nil nil "-q"))
        (dolist (file key-files)
          (when (file-exists-p (expand-file-name file))
            (call-process "ssh-add" nil nil nil (expand-file-name file)))))))
    (message "SSH keys added")))

(map! :leader "a" nil)

(map! :leader
      :desc "embark act" "e" #'embark-act)

(map! :leader
      :prefix ("a" . "actions")
      :desc "ensure ssh auto-closes opener socket" "O" #'my/configure-opener
      :desc "imapfilter" "i" #'my/imapfilter
      :desc "agent shell" "a" #'agent-shell
      (:prefix ("u" . "updates")
       :desc "nix rebuild" "n" #'my/smart-rebuild
       :desc "doom sync" "d" #'my/doom-sync
       :desc "add ssh keys" "k" #'my/ssh-add
       :desc "refresh ssh auth socket" "a" #'ssh-auth-sock-update)
      :desc "shelldon output history" "h" #'shelldon-output-history
      :desc "dismiss shelldon" "d" #'my/shelldon-dismiss)

(defun gib (n)
  (* n 1024 1024 1024))

(defun mib (n)
  (* n 1024 1024))

(with-eval-after-load 'gcmh
  (setq gcmh-high-cons-threshold (gib 1))
  (setq gcmh-low-cons-threshold  (mib 300)))

(use-package json-mode
  :mode ("\\.hujson\\'" . jsonc-mode))

(use-package kdl-mode
  :mode ("\\.kdl\\'" . kdl-mode))

(use-package web-mode
  :mode ("\\.tt" . web-mode))

(use-package ssh-config-mode
  :mode (("/\\.?ssh/config\\(\\.d/.*\\)?\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

(setq +format-on-save-enabled-modes
      '(not json-mode jsonc-mode fish-mode))

(setq projectile-project-search-path
      '(("~/Git" . 2)))

(setq projectile-project-root-functions
      '(projectile-root-local
        projectile-root-marked
        projectile-root-top-down
        projectile-root-bottom-up
        projectile-root-top-down-recurring))

                                        ; (def-projectile-commander-method ?e "eshell" (+eshell/frame))

(defun my/short-system-name ()
  (seq-take-while (lambda (elt) (not (eq elt ?.))) (system-name)))

(setq frame-title-format
      '((:eval (if (getenv "SSH_CONNECTION") (concat "[" (my/short-system-name) "] ")))
        "emacs %b"))

(setq bookmark-default-file "~/.doom.d/bookmarks")
(setq bookmark-save-flag 1)

(setq recentf-max-saved-items 400)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk.  Use
;; auto-revert-mode to revert a particular buffer.
(global-auto-revert-mode 1)

;; also enable this for non-file buffers, like dired
(setq global-auto-revert-non-file-buffers t)

(menu-bar-mode -1)

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode)))

  (defun eshell/emacs (file)
    (find-file file))

  (defun eshell/vi (file)
    (find-file file))

  (defun eshell/vim (file)
    (find-file file))

  (set-eshell-alias!
   "home" "cd ~/Git/dylanwh/dotfiles"
   "pull" "git pull"
   "push" "git push"
   "full-disk-access-p" "plutil -lint /Library/Preferences/com.apple.TimeMachine.plist"
   "eza" "eza -F -h --group-directories-first -b --smart-group -soldest -r $*")

  )

;; for some reason, emacsformacosx has scratch buffer not being in lisp-interactive-mode?
(defun fix-scratch-buffer (&rest _)
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (unless (eq major-mode 'lisp-interaction-mode)
        (lisp-interaction-mode)))))

(advice-add 'scratch-buffer :after #'fix-scratch-buffer)

(if (version< emacs-version "30.1")
    (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-use-connection-share t))

;; Headscale SSH prints a login URL and waits for it to be visited before
;; giving us a shell. TRAMP doesn't know that prompt, so it sits there silently
;; until the connection times out. Teach it to show the URL (as text and as a
;; QR code), open it, and wait for ssh to say something else.
(with-eval-after-load 'tramp-sh
  (defconst my/tramp-headscale-auth-regexp
    (rx "To authenticate, visit: " (group "http" (+ (not space))) (* space)
        ;; Headscale prints this right after the URL, *before* the check
        ;; actually passes, so it is what ends the buffer.
        (? "# Authentication checked" (* nonl) (* space))))

  (defun my/tramp-action-headscale-auth (proc vec)
    "Show the Headscale SSH login URL, then wait for new output from PROC."
    (require 'qrencode)
    (let ((url (with-current-buffer (process-buffer proc)
                 (goto-char (point-min))
                 (tramp-check-for-regexp proc my/tramp-headscale-auth-regexp)
                 (match-string 1))))
      (tramp-message vec 3 "Headscale SSH: authenticate at %s" url)
      (let ((buf (get-buffer-create "*Headscale SSH*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert url "\n\n"
                    (propertize (qrencode-format (qrencode url nil nil 'return-raw))
                                'face 'qrencode-face))
            (special-mode)))
        ;; A centered child frame sized to the QR code.
        (posframe-show buf
                       :poshandler #'posframe-poshandler-frame-center
                       :border-width 1)
        (with-current-buffer (process-buffer proc)
          (unwind-protect
              ;; Suspend timers so `tramp-connection-timeout' can't fire while
              ;; we wait on the user, as TRAMP itself does for security-key
              ;; prompts.
              (let (tramp-dont-suspend-timers)
                (with-tramp-suspended-timers
                  (with-temp-message "Headscale SSH: waiting for you to authenticate (C-g to abort)"
                    (redisplay 'force)
                    ;; Hide the banner from later actions; wait for any new
                    ;; output.
                    (narrow-to-region (point-max) (point-max))
                    (tramp-wait-for-regexp proc nil (rx anychar)))))
            (posframe-delete buf)))))
    t)

  (add-to-list 'tramp-actions-before-shell
               '(my/tramp-headscale-auth-regexp my/tramp-action-headscale-auth)))

;; With kkp active the terminal sends C-g as an escape sequence, which Emacs
;; only understands as a key at the command loop, not as a quit while it's
;; busy. Fall back to legacy key encoding while TRAMP waits for login prompts
;; (passwords, the Headscale check above, ...) so C-g can abort a stuck
;; connection.
(with-eval-after-load 'kkp
  (advice-add 'tramp-process-actions :around #'kkp-restore-legacy-keys))

(with-eval-after-load 'evil
  (setq evil-move-beyond-eol nil
        evil-track-eol t
        evil-v$-excludes-newline t
        evil-split-window-below t
        evil-vsplit-window-right t
        hscroll-margin 2
        hscroll-step 1)
  (evil-ex-define-cmd "tangle" 'org-babel-tangle)
  (evil-ex-define-cmd "scratch" 'scratch-buffer)
  (evil-ex-define-cmd "q" 'bury-buffer)
  (evil-ex-define-cmd "wq" 'doom/save-and-kill-buffer))

(defun my/pinentry--root-frame (frame)
  "Return the top-level ancestor of FRAME."
  (while (frame-parent frame)
    (setq frame (frame-parent frame)))
  frame)

(defun my/pinentry--child-frame-prompt (orig labels query-function &rest query-args)
  "Show the pinentry prompt in a centered child frame with its own minibuffer.
Falls back to ORIG when child frames are unavailable."
  (if (not (or (display-graphic-p) (featurep 'tty-child-frames)))
      (apply orig labels query-function query-args)
    (let* ((title (cdr (assq 'title labels)))
           (desc (cdr (assq 'desc labels)))
           (err (cdr (assq 'error labels)))
           (prompt (cdr (assq 'prompt labels)))
           (parent (my/pinentry--root-frame (selected-frame)))
           (orig-frame (selected-frame))
           frame)
      (when (and title desc)
        (setq desc (concat title "\n" desc)))
      (when err
        (setq desc (concat "Error: " (propertize err 'face 'error)
                           (if desc (concat "\n" desc) ""))))
      (when (and prompt (string-match "[ \n]*\\'" prompt))
        (setq prompt (concat (substring prompt 0 (match-beginning 0)) " ")))
      (let* ((lines (if desc (split-string desc "\n") nil))
             (max-width (max 20 (- (frame-width parent) 4)))
             (width (min max-width
                         (max 46
                              (+ (length prompt) 30)
                              (apply #'max 0 (mapcar #'string-width lines)))))
             (height (1+ (length lines)))
             (left (max 0 (/ (- (frame-width parent) width) 2)))
             (top (max 0 (/ (- (frame-height parent) height) 2)))
             (after-make-frame-functions nil))
        (unless (and pinentry--prompt-buffer
                     (buffer-live-p pinentry--prompt-buffer))
          (setq pinentry--prompt-buffer (generate-new-buffer "*Pinentry*")))
        (when desc
          (with-current-buffer pinentry--prompt-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert desc))
            (pinentry-prompt-mode)
            (setq-local cursor-type nil)
            (goto-char (point-min))))
        (setq frame
              (make-frame
               `((parent-frame . ,parent)
                 (minibuffer . ,(if desc t 'only))
                 ;; On a tty an undecorated child frame has no border.
                 (undecorated . ,(display-graphic-p))
                 (child-frame-border-width . 1)
                 (internal-border-width . 1)
                 (left . ,(* left (frame-char-width parent)))
                 (top . ,(* top (frame-char-height parent)))
                 (width . ,width)
                 (height . ,height)
                 (no-other-frame . t)
                 (unsplittable . t)
                 (menu-bar-lines . 0)
                 (tool-bar-lines . 0)
                 (tab-bar-lines . 0)
                 (vertical-scroll-bars . nil)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (desktop-dont-save . t))))
        (when desc
          (let ((win (frame-root-window frame)))
            (set-window-parameter win 'mode-line-format 'none)
            (set-window-parameter win 'header-line-format 'none)
            (set-window-buffer win pinentry--prompt-buffer)
            (set-window-dedicated-p win t)))
        ;; `pinentry--process-filter' keeps running in its process buffer after
        ;; we return, and `select-frame' switches the current buffer; restore
        ;; it, as `save-window-excursion' does in the original.
        ;; The description window is one line tall; without this, exiting the
        ;; first minibuffer of a repeat prompt grows it to `window-min-height'.
        (save-current-buffer
          (let ((window-min-height 1))
            (unwind-protect
                (progn
                  (select-frame-set-input-focus frame)
                  (apply query-function prompt query-args))
              (when (frame-live-p frame)
                (delete-frame frame t))
              (when (frame-live-p orig-frame)
                (select-frame-set-input-focus orig-frame)))))))))

(use-package pinentry
  :config
  (advice-add 'pinentry--prompt :around #'my/pinentry--child-frame-prompt)
  (pinentry-start))

(use-package age
  :custom
  (age-program "rage")
  (age-default-identity
   (cl-find-if #'file-readable-p
               '( "~/.ssh/id_ed25519" "~/.ssh/id_rsa")))
  (age-default-recipient
   '("~/.ssh/authorized_keys"))
  :config
  (setenv "PINENTRY_PROGRAM" "pinentry-emacs")
  (age-file-enable))

(setq shell-file-name (executable-find "bash"))

(let ((fish (executable-find "fish")))
  (setq-default ghostel-shell fish)
  (setq-default vterm-shell fish)
  (setq-default explicit-shell-file-name fish))

(defun treesit-show-parser-used-at-point ()
  "Shows treesit parser used at point."
  (interactive)
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      (message (format "%s" (treesit-language-at (point))))
    (message "treesit is not available")))

(use-package agent-shell
  :config
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-header-style nil)
  (setq agent-shell-inhibit-system-sleep nil)

  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  (defun my/agent-shell-diff-emacs-state ()
    "Use Emacs state in agent-shell diff buffers."
    (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
      (evil-emacs-state)))
  (add-hook 'diff-mode-hook #'my/agent-shell-diff-emacs-state))

(with-eval-after-load 'agent-shell
  (map! :map agent-shell-mode-map
        :i "RET" #'newline
        :n "RET" #'comint-send-input))

(setq code-review-auth-login-marker 'forge)

(defun my/browse-url-remote-opener (url &optional _new-window)
  (let ((socket-path (expand-file-name "~/.opener.sock")))
    (condition-case nil
        (let ((proc (make-network-process
                     :name "opener-client"
                     :family 'local
                     :service socket-path
                     :buffer nil
                     :noquery t)))
          (process-send-string proc (concat url "\n"))
          (delete-process proc)
          (message "opener: %s" url)
          t)
      
      (error (message "opener not available, url: %s" url)
             nil))))

;; Set it as the default browser for Emacs
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function 'my/browse-url-remote-opener))

;; eww: identify as a real browser, route some hosts through the gothub/redlib
;; frontends, and render Markdown responses (via pandoc) instead of raw text.
(setq url-user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0")

(with-eval-after-load 'eww
  (defvar my/eww-host-rewrites
    '(("github.com" . "github.on.default.horse")
      ("reddit.com" . "reddit.on.default.horse")))

  (defun my/eww-rewrite-host (url)
    (let ((u (url-generic-parse-url url)))
      (when-let ((new (cdr (assoc (url-host u) my/eww-host-rewrites))))
        (setf (url-host u) new))
      (url-recreate-url u)))

  (defun my/eww-github-blob-to-raw (url)
    "Rewrite GitHub/gothub blob URLs for Markdown files to their raw form."
    (let ((u (url-generic-parse-url url)))
      (when (and (member (url-host u) '("github.com" "github.on.default.horse"))
                 (string-match
                  "\\`/\\([^/]+/[^/]+\\)/blob/\\(.+\\.\\(?:md\\|markdown\\)\\)\\(\\?.*\\)?\\'"
                  (url-filename u)))
        (setf (url-host u) "github.on.default.horse")
        (setf (url-filename u)
              (replace-match "/raw/\\1/\\2\\3" t nil (url-filename u))))
      (url-recreate-url u)))

  (setq eww-url-transformers
        (append (default-value 'eww-url-transformers)
                (list #'my/eww-rewrite-host #'my/eww-github-blob-to-raw)))

  (setq eww-readable-urls
        '("\\`https?://[^/]*\\(medium\\.com\\|substack\\.com\\)/"
          "\\`https?://support\\.apple\\.com"))

  (defun my/eww-markdown-response-p ()
    "Non-nil if the current url-retrieve buffer holds Markdown."
    (let ((type (and (boundp 'url-http-content-type) url-http-content-type))
          (path (and url-current-object (url-filename url-current-object))))
      (or (and type (string-match-p "\\`text/\\(x-\\)?markdown" type))
          (and path (string-match-p "\\.\\(md\\|markdown\\)\\(\\?.*\\)?\\'" path)))))

  (defun my/eww-display-markdown (orig-fn buffer &optional encode)
    (if (and url-current-object
             (my/eww-markdown-response-p)
             (executable-find "pandoc"))
        (let ((url (url-recreate-url url-current-object))
              (start (point))
              (coding-system-for-read 'no-conversion)
              (coding-system-for-write 'no-conversion))
          (call-process-region start (point-max) "pandoc" t t nil
                               "-f" "gfm" "-t" "html")
          (goto-char start)
          (eww-display-html 'utf-8 url nil nil buffer))
      (funcall orig-fn buffer encode)))

  (advice-add 'eww-display-raw :around #'my/eww-display-markdown))

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 ;; mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"

 mu4e-headers-hide-predicate
 (lambda (msg)
   (member 'trashed (mu4e-message-field msg :flags)))
 
 mu4e-bookmarks
 (let ((exclude (concat "AND NOT \"maildir:/Junk Mail\""
                        " AND NOT maildir:/Trash"
                        " AND NOT maildir:/Bugzilla"
                        " AND NOT maildir:/+SaneBlackhole"
                        " AND NOT maildir:/GMail")))
   `((:name "Unread messages"
      :query ,(concat "flag:unread " exclude)
      :key ?u)
     (:name "Today's messages"
      :query ,(concat "date:today..now " exclude)
      :key ?t)
     (:name "Last 7 days"
      :query ,(concat "date:7d..now "
                      "AND NOT \"maildir:/Junk Mail\" "
                      "AND NOT maildir:/Trash "
                      "AND NOT maildir:/+SaneBlackhole")
      :hide-unread t
      :key ?w)
     (:name "Messages with images"
      :query "mime:image/* AND NOT maildir:/+SaneBlackhole"
      :key ?p)
     (:name "Flagged messages"
      :query ,(concat "flag:flagged "
                      "AND NOT \"maildir:/Junk Mail\" "
                      "AND NOT maildir:/Trash "
                      "AND NOT maildir:/+SaneBlackhole")
      :key ?f)))

 mu4e-maildir-shortcuts
 '((:maildir "/INBOX"     :key ?i)
   (:maildir "/+SaneLater" :key ?l :hide-if-no-unread t)
   (:maildir "/+SaneNews"  :key ?n :hide-if-no-unread t)
   (:maildir "/Archive"   :key ?a :hide-if-no-unread t)
   (:maildir "/Billing"   :key ?b :hide-if-no-unread t)
   (:maildir "/Bugzilla"  :key ?z :hide-if-no-unread t)
   (:maildir "/Junk Mail" :key ?j)
   (:maildir "/Trash"     :key ?t)))

(map! :desc "Open config.el" :ni "s-," (cmd! (find-file (expand-file-name "config.el" doom-user-dir))))

(map! :leader :desc "Elfeed" :n "o n" #'elfeed)
(with-eval-after-load 'elfeed
  (map! :after elfeed
        :map elfeed-search-mode-map
        :leader :desc "Elfeed update" :n "o u" #'elfeed-update)

  (defvar my/elfeed-ignored-tags '(elfeed unread starred)
    "Tags to exclude from the elfeed tag filter list.")

  (defun my/elfeed-db-tags ()
    "Return all unique tags from the elfeed database, excluding `my/elfeed-ignored-tags'."
    (let ((tags (make-hash-table :test 'eq)))
      (with-elfeed-db-visit
       (entry _feed)
       (dolist (tag (elfeed-entry-tags entry))
         (unless (memq tag my/elfeed-ignored-tags)
           (puthash tag t tags))))
      (hash-table-keys tags)))

  (defun my/elfeed-unread-count (tag)
    "Return the number of unread elfeed entries with TAG."
    (let ((count 0))
      (with-elfeed-db-visit
       (entry _feed)
       (when (and (memq 'unread (elfeed-entry-tags entry))
                  (memq tag (elfeed-entry-tags entry)))
         (cl-incf count)))
      count))

  (defun my/elfeed-tag-filter ()
    "Select an elfeed tag filter via completing-read."
    (interactive)
    (let* ((tags (my/elfeed-db-tags))
           (candidates
            (cl-loop for tag in (sort tags (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     for count = (my/elfeed-unread-count tag)
                     when (> count 0)
                     collect (cons (format "%s (%d)" (symbol-name tag) count)
                                   tag)))
           (candidates (cons '("All unread" . nil) candidates)))
      (if (= (length candidates) 1)
          (message "No unread entries.")
        (let* ((choice (completing-read "Elfeed filter: " (mapcar #'car candidates) nil t))
               (tag (cdr (assoc choice candidates))))
          (elfeed-search-set-filter
           (if tag
               (format "+unread +%s" (symbol-name tag))
             "+unread"))))))

  (map! :map elfeed-search-mode-map
        :n "T" #'my/elfeed-tag-filter))

(defun my/notify-new-buffer ()
  "Send an OSC 777 notification for new emacsclient buffers."
  (when-let* ((tty (terminal-name (frame-terminal)))
              (buf (buffer-name))
              (_ (not (string-match-p "\\`COMMIT_EDITMSG\\'" buf))))
    (send-string-to-terminal
     (format "\e]777;notify;emacs;%s\e\\" buf)
     (frame-terminal))))

(add-hook 'server-visit-hook #'my/notify-new-buffer)

;; pretty proced
(with-eval-after-load 'proced
  (setq proced-enable-color-flag t
        proced-format 'medium
        proced-tree-flag t
        proced-auto-update-flag 'visible
        proced-auto-update-interval 5))

(setq doom-old-hope-brighter-comments t)
(setq doom-old-hope-brighter-modeline t)
(setq doom-outrun-electric-brighter-comments t)
(setq doom-outrun-electric-brighter-modeline t)

(defvar my/host-theme-alist
  '(("bragi" . doom-moonlight)
    ("frigg" . doom-old-hope)
    ("odin" . doom-outrun-electric)
    ("jord" . modus-vivendi-tinted))
  "Alist mapping hostnames to theme names.") 

(standard-display-unicode-special-glyphs)

(defun my/syntax-init-popups-tty ()
  "Prefer flycheck-posframe wherever posframe works, including tty child frames."
  (unless (and (bound-and-true-p lsp-ui-mode) lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode) (posframe-workable-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

(advice-add '+syntax-init-popups-h :override #'my/syntax-init-popups-tty)

(with-eval-after-load 'flycheck-posframe
  (setq flycheck-posframe-border-width 1))

;; Load local configuration if it exists
(let ((local-config (expand-file-name "local-config.el" doom-user-dir)))
  (when (file-exists-p local-config)
    (load local-config)))

(let ((theme (or (alist-get (my/short-system-name)
                            my/host-theme-alist
                            nil nil #'string=) 'doom-dark+)))
  (load-theme theme t))

(require 'server)
(unless (server-running-p)
  (server-start))
