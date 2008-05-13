;; ---------------------------------------------------------------------------
;; Hot keys (print this information for reference):
;; ---------------------------------------------------------------------------
;;          F1 - Goto line
;;          F2 - Comment highlighted region (highlight a region first!)
;;          F3 - Switch to a command prompt shell
;;          F4 - Correctly indent a highlighted region (highlight a region first!)
;;          F5 - Goto next window (buffer)
;;          F8 - Run the current perl code

;; ------------------------------------------------------------
;; From Perl Hacks

;; autocomplete for variables with tab; needs Perl::Tidy installed
(defadvice cperl-indent-command
  (around cperl-indent-or-complete)
  "Changes \\[cperl-indent-or-complete] so it autocompletes when at the end of a word."
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    ad-do-it))
(eval-after-load "cperl-mode"
  '(progn (require 'dabbrev)(ad-activate 'cperl-indent-command)))

;; perltidy region with <strg><c> <t>
(defmacro mark-active ()
  "Xemacs/emacs compatibility macro"
  (if (boundp 'mark-active)
      'mark-active
    '(mark)))
(defun perltidy ()
  "execute perltidy for the selected region or the current buffer"
  (interactive)
  ; save-excursion doesn't work somehow... so:
  (let ((orig-point (point)))
    (unless (mark-active) (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    (goto-char orig-point)))
(global-set-key "\C-ct" 'perltidy)

;; run selected region as perl code with <Strg><Alt><p>
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (shell-command-on-region beg end "perl")
  ; feeds the region to perl on STDIN
  )
(global-set-key "\M-\C-p" 'perl-eval)


;; ------------------------------------------------------------
;; From: Perl Best Practices (D. Conway)

;; Use cperl mode instead of the default perl mode
(defalias 'perl-mode 'cperl-mode)

;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)

;; Use 4 space indents via cperl mode
(custom-set-variables
  '(cperl-close-paren-offset -4)
  '(cperl-continued-statement-offset 4)
  '(cperl-indent-level 4)
  '(cperl-indent-parens-as-block t)
  '(cperl-tab-always-indent t)
)

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set line width to 78 columns
(setq fill-column 79)
(setq auto-fill-mode t)

;; ------------------------------------------------------------
;; Steve Ackermann

;; colors
(require 'font-lock)
(global-font-lock-mode t)

;; The mode line (bar at the bottom)
(add-hook 'font-lock-mode-hook
	  '(lambda ()
             (set-face-background 'modeline            "Blue4")
             (set-face-foreground 'modeline            "Gold")
; 	     (set-face-foreground 'secondary-selection "red")
;            (set-face-background 'highlight           "yellow")
))

;; Background
;;(set-background-color "white smoke")

;;Comments in italics
(setq w32-enable-italics t)
(make-face-italic 'font-lock-comment-face)

;; Override default text colours
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(font-lock-comment-face ((((class color)) (:foreground "green4"))))
 '(font-lock-function-name-face ((((class color)) (:foreground "firebrick"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Purple"))))
 '(font-lock-type-face ((((class color)) (:foreground "blue"))))
 '(region ((((class color)) (:background "wheat"))))
 '(show-paren-mismatch-face ((((class color)) (:background "purple" :foreground "white"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red" :underline "black")))))

;; highlight region between point and mark
(transient-mark-mode t)
;; highlight during query
(setq query-replace-highlight t)        
;; highlight incremental search
(setq search-highlight t)


; don't make pesky backup files
(setq make-backup-files nil)
; don't use version numbers for backup files
(setq version-control 'never)

;; Don't want "//" to bugger things up in a filename.
(setq filename-handler-alist nil)

;; Make searches case insensitive
(setq case-fold-search t)

;; Make control+pageup/down scroll the other buffer
(global-set-key [(control next)] 'scroll-other-window)
(global-set-key [(control prior)] 'scroll-other-window-down)

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parenthesis. How can you live without it.
(show-paren-mode t)

;; Open unidentified files in text mode
(setq default-major-mode 'text-mode)

;; Set titles for frame and icon (%f == file name, %b == buffer name)
(setq-default frame-title-format (list "Emacs: %f"))
(setq-default icon-title-format "Emacs - %b")

;; Place Emacs in the location (0, 0) on screen 
(setq initial-frame-alist
  '(
     (top               . 0)
     (left              . 0)
  )
)

;; Don't add new lines to the end of a file when using down-arrow key
(setq next-line-add-newlines nil)

;; Do only one line scrolling.
(setq scroll-step 1)

;; Don't wrap long lines.
;;(set-default 'truncate-lines t)
;; Nevertheless I'd like to have the possibility to see what is out of my view.
(require 'auto-show)
(auto-show-mode 1)
(setq-default auto-show-mode t)

;; Current line & column of cursor in the mode line (bar at the bottom)
(line-number-mode 1)
(setq column-number-mode t)
;; show current function in modeline
(which-func-mode t)                 

;; Key Bindings
;; Windows-like selection and key bindings, but don't replace marked text when writing
(pc-bindings-mode)
(pc-selection-mode)
(delete-selection-mode nil)

(setq my-author-name (getenv "USER"))
(setq user-full-name (getenv "USER"))
(setq default-directory "E:/")

;; Perl-Menu
(define-key global-map [menu-bar perl-menu]
  (cons "Perl" (make-sparse-keymap "Perl")))
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [goto-line-label] '("Goto Line" . goto-line) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [comment-region-label] '("Comment Highlighted Region" . comment-region) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [shell-label] '("MS-DOS Command Prompt" . shell) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [indent-region-label] '("Indent Highlighted Region            (<f4>)" . indent-region) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [bury-buffer-label] '("Previous Window" . bury-buffer) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [wrap-all-lines-label] '("Wrap Lines" . wrap-all-lines) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [run-perl-label] '("Run Current Perl Code" . run-perl) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [unix-to-dos-label] '("Reformat UNIX -> DOS" . unix-dos) t)
(define-key-after (lookup-key global-map [menu-bar perl-menu])
  [dos-unix-label] '("Reformat DOS -> UNIX" . dos-unix) t)

;; PC Function Keys
(global-set-key [f1] 'goto-line) 
(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'shell)
(global-set-key [f4] 'indent-region)
(global-set-key [f5] 'bury-buffer)

(global-set-key [f8] 'run-perl) 

;; ------------------------------------------------------------
;; own enhancements
;; auto-insert for .pl and .pm and .ptk
(load-library "autoinsert")


(define-auto-insert "\\.pl$" 'perl-auto-insert)
(defun perl-auto-insert ()
  (progn
    (insert "#! /usr/bin/perl\nuse warnings;\nuse strict;\n")
    (insert "use Data::Dumper;\n$Data::Dumper::Indent   = 1;\n")
    (insert "$Data::Dumper::Sortkeys = 1;\n\n")
    )
  )

(define-auto-insert "\\.cgi$" 'cgi-auto-insert)
(defun cgi-auto-insert ()
  (progn
    (insert "#! /usr/bin/perl\nuse warnings;\nuse strict;\n\n")
    (insert "use CGI ();\n")
    (insert "use CGI::Carp qw(fatalsToBrowser);\n")
    (insert "use HTML::Template::Compiled;\n")
    (insert "use FindBin ();\n")
    (insert "use lib $FindBin::Bin;\n\n")
    (insert "my $filename = 'template.tmpl';\n")
    (insert "my $cssUrl   = 'style.css';\n\n");
    (insert "my $cgi    = CGI->new();\nmy %params = $cgi->Vars();\n\n")
    (insert "print $cgi->header( -type => 'text/html', -expires => '+3s' );\n")
    (insert "my $constructorParams = { filename => $filename,\n")
    (insert "                          path     => \"$FindBin::Bin/templates\",\n")
    (insert "                          # die_on_bad_params => 0,\n")
    (insert "                         };\n\n")
    (insert "my $template = &Template\n")
    (insert "    ( $cgi, $constructorParams,\n")
    (insert "      {\n       SELF_URL => $ENV{SCRIPT_NAME},\n")
    (insert "       CSS_URL => $cssUrl,\n   } );\n\n")
    (insert "print $template->output();\n\n")
    (insert "# print $cgi->start_html('title');\n\n\n")
    (insert "# print $cgi->end_html();\n\n")
    (insert "# ------------------------------------------------------------\n")
    (insert "sub Template {\n")
    (insert "    my( $cgi, $constructorParams, $htmlParams ) = @_;\n\n")
    (insert "    my $template = HTML::Template::Compiled->new( %$constructorParams );\n")
    (insert "    $template->param( %$htmlParams ) if ref $htmlParams;\n\n")
    (insert "    return $template;\n")
    (insert "} # Template\n")
    (insert "# ------------------------------------------------------------\n")
    )
  )

(define-auto-insert "\\.pm$" 'pm-auto-insert)
(defun pm-auto-insert ()
  (progn
    (insert "package ")
;;    (insert (file-name-nondirectory buffer-file-name))
    (insert (substring (file-name-nondirectory buffer-file-name) 0 -3))
    (insert ";\nuse warnings;\nuse strict;\n")
    (insert "use Carp;\n\n")
    (insert "use Readonly;\n\n\n")
    (insert "# ------------------------------------------------------------\n\n")
    (insert "sub {\n\n\n} #\n")
    (insert "# ------------------------------------------------------------\n")
    (insert "1; # modules have to return a true value\n")
  )
)

(define-auto-insert "\\.ptk$" 'ptk-auto-insert)
(defun ptk-auto-insert ()
  (progn
    (insert "#! /usr/bin/perl\nuse warnings;\nuse strict;\n\n")
    (insert "use Tk ();\nuse Tk::Dialog ();\n\n");
    (insert "use vars qw($Status $Mw);\n$Status = '';\n\n");
    (insert "$Mw = &BuildMainWindow(\\$Status);\n\n\n")
    (insert "&Tk::MainLoop;\n\n")
    (insert "# ------------------------------------------------------------\n")
    (insert "sub BuildMainWindow {\n    my ($status) = @_;\n\n")
    (insert "    my $mw = MainWindow->new();\n")
    (insert "    $mw->protocol('WM_DELETE_WINDOW', [ \\&ExitApplication, $mw] );\n")
    (insert "    my $statusFrame = $mw->Frame(-relief => 'ridge', -border => 1)\n")
    (insert "        ->pack(-side => 'bottom', -fill => 'x');\n")
    (insert "    $statusFrame->Label(-text => 'Status: ')     ->pack(-side => 'left');\n")
    (insert "    $statusFrame->Label(-textvariable => $status)->pack(-side => 'left');\n\n")
    (insert "    my $frame = $mw->Frame()\n")
    (insert "        ->pack(-fill => 'both', -expand => 1);\n\n")
    (insert "    return $mw;\n} # BuildMainWindow\n")
    (insert "# ------------------------------------------------------------\n")
    (insert "sub ExitApplication {\n")
    (insert "    my ($mw) = @_;\n")
    (insert "    my $dialog = $mw->Dialog(-text => 'Programm wirklich beenden?',\n") 
    (insert "                             -bitmap => 'question',\n")
    (insert "                             -title => 'Programm beenden',\n")
    (insert "                             -default_button => 'Yes',\n")
    (insert "                             -buttons => [qw/Ja Nein/],\n")
    (insert "                             );\n\n")
    (insert "    my $answer = $dialog->Show(); # and display dialog\n")
    (insert "    if( lc($answer) eq 'ja' ){\n        exit;\n    }\n    else {\n")
    (insert "        # continue\n    } # else\n\n} # ExitApplication\n")
    (insert "# ------------------------------------------------------------\n")
    )
  )

(add-hook 'find-file-hooks 'auto-insert)
(desktop-load-default)
(desktop-read)

;;Add a variable and function index to the menu
(require 'imenu)
(add-hook 'c-mode-hook (function (lambda () (imenu-add-to-menubar 'Func))))
(add-hook 'c++-mode-hook (function (lambda () (imenu-add-menubar-index))))
(add-hook 'perl-mode-hook (function (lambda () (imenu-add-menubar-index))))
(add-hook 'perl-mode-hook (function (lambda () (imenu-add-to-menubar 'Func))))
(add-hook 'cperl-mode-hook (function (lambda () (imenu-add-menubar-index))))
(add-hook 'cperl-mode-hook (function (lambda () (imenu-add-to-menubar 'Func))))
(add-hook 'java-mode-hook (function (lambda () (imenu-add-menubar-index))))


;; ------------------------------------------------------------
;; FUNCTIONS
;;Convert DOS cr-lf to UNIX newline
(defun dos-unix () (interactive) (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
;;Convert UNIX newline to DOS cr-lf
(defun unix-dos () (interactive) (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;make-comment-italic
(defun make-comment-italic ()
  (interactive "*")
  (make-face-italic 'font-lock-comment-face))

;;make-comment-unitalic
(defun make-comment-unitalic ()
  (interactive "*")
  (make-face-unitalic 'font-lock-comment-face))

;;make-comment-invisible
(defun make-comment-invisible ()
  (interactive "*")
  (custom-set-faces
 '(font-lock-comment-face ((((class color)) (:foreground "white"))))))

;;make-comment-visible
(defun make-comment-visible ()
  (interactive "*")
  (custom-set-faces
 '(font-lock-comment-face ((((class color)) (:foreground "green4"))))))

;;make-comment-red
(defun make-comment-red ()
  (interactive "*")
  (custom-set-faces
 '(font-lock-comment-face ((((class color)) (:foreground "red3"))))))

(defun wrap-all-lines ()
  "Enable line wrapping"
  (interactive) ;this makes the function a command too
  (set-default 'truncate-lines nil)
)

;;run the current perl program
(defun run-perl ()
  (interactive "*")
  (setq perl-buffer-name buffer-file-name)
  (shell)
  (setq perl-run-command "perl ")
  (insert perl-run-command)
  (insert perl-buffer-name)
)

;;debug the current perl program
(defun debug-perl ()
  (interactive "*")
  (setq perl-buffer-name buffer-file-name)
  (shell)
  (setq perl-run-command "perl -d ")
  (insert perl-run-command)
  (insert perl-buffer-name)
)


;; ---------------------------------------------------------------------------
;; Bind major editing modes to certain file extensions 
;;----------------------------------------------------------------------------
(setq auto-mode-alist
      '(("\\.[Cc][Oo][Mm]\\'" . text-mode)
        ("\\.bat\\'" . bat-generic-mode)
        ("\\.inf\\'" . inf-generic-mode)
        ("\\.rc\\'" . rc-generic-mode)
        ("\\.reg\\'" . reg-generic-mode)
        ("\\.cob\\'" . cobol-mode)
        ("\\.cbl\\'" . cobol-mode)
        ("\\.te?xt\\'" . text-mode)
        ("\\.c\\'" . c-mode)
        ("\\.h\\'" . c++-mode)
        ("\\.tex$" . LaTeX-mode)
        ("\\.sty$" . LaTeX-mode)
        ("\\.bbl$" . LaTeX-mode)
        ("\\.bib$" . BibTeX-mode)
        ("\\.el\\'" . emacs-lisp-mode)
        ("\\.scm\\'" . scheme-mode)
        ("\\.l\\'" . lisp-mode)
        ("\\.lisp\\'" . lisp-mode)
        ("\\.f\\'" . fortran-mode)
        ("\\.F\\'" . fortran-mode)
        ("\\.for\\'" . fortran-mode)
        ("\\.p\\'" . pascal-mode)
        ("\\.pas\\'" . pascal-mode)
        ("\\.ad[abs]\\'" . ada-mode)
        ("\\.\\([pP][Llm]\\|al\\)\\'" . perl-mode)
	("\\.ptk$" . perl-mode)
	("\\.cgi$"  . perl-mode)
        ("\\.s?html?\\'" . html-mode)
        ("\\.ttml?\\'" . html-mode)
        ("\\.idl\\'" . c++-mode)
        ("\\.cc\\'" . c++-mode)
        ("\\.hh\\'" . c++-mode)
        ("\\.hpp\\'" . c++-mode)
        ("\\.C\\'" . c++-mode)
        ("\\.H\\'" . c++-mode)
        ("\\.cpp\\'" . c++-mode)
        ("\\.[cC][xX][xX]\\'" . c++-mode)
        ("\\.hxx\\'" . c++-mode)
        ("\\.c\\+\\+\\'" . c++-mode)
        ("\\.h\\+\\+\\'" . c++-mode)
        ("\\.m\\'" . objc-mode)
        ("\\.java\\'" . java-mode)
        ("\\.ma?k\\'" . makefile-mode)
        ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
        ("\\.am\\'" . makefile-mode)
        ("\\.mms\\'" . makefile-mode)
        ("\\.texinfo\\'" . texinfo-mode)
        ("\\.te?xi\\'" . texinfo-mode)
        ("\\.s\\'" . asm-mode)
        ("\\.S\\'" . asm-mode)
        ("\\.asm\\'" . asm-mode)
        ("ChangeLog\\'" . change-log-mode)
        ("change\\.log\\'" . change-log-mode)
        ("changelo\\'" . change-log-mode)
        ("ChangeLog\\.[0-9]+\\'" . change-log-mode)
        ("changelog\\'" . change-log-mode)
        ("changelog\\.[0-9]+\\'" . change-log-mode)
        ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
        ("\\.scm\\.[0-9]*\\'" . scheme-mode)
        ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
        ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
        ("\\(/\\|\\`\\)\\.\\(bash_logout\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
        ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
        ("\\.mm\\'" . nroff-mode)
        ("\\.me\\'" . nroff-mode)
        ("\\.ms\\'" . nroff-mode)
        ("\\.man\\'" . nroff-mode)
        ("\\.[12345678]\\'" . nroff-mode)
        ("\\.TeX\\'" . TeX-mode)
        ("\\.sty\\'" . LaTeX-mode)
        ("\\.cls\\'" . LaTeX-mode)
        ("\\.clo\\'" . LaTeX-mode)
        ("\\.bbl\\'" . LaTeX-mode)
        ("\\.bib\\'" . BibTeX-mode)
        ("\\.m4\\'" . m4-mode)
        ("\\.mc\\'" . m4-mode)
        ("\\.mf\\'" . metafont-mode)
        ("\\.mp\\'" . metapost-mode)
        ("\\.vhdl?\\'" . vhdl-mode)
        ("\\.article\\'" . text-mode)
        ("\\.letter\\'" . text-mode)
        ("\\.tcl\\'" . tcl-mode)
        ("\\.exp\\'" . tcl-mode)
        ("\\.itcl\\'" . tcl-mode)
        ("\\.itk\\'" . tcl-mode)
        ("\\.icn\\'" . icon-mode)
        ("\\.sim\\'" . simula-mode)
        ("\\.mss\\'" . scribe-mode)
        ("\\.f90\\'" . f90-mode)
        ("\\.lsp\\'" . lisp-mode)
        ("\\.awk\\'" . awk-mode)
        ("\\.prolog\\'" . prolog-mode)
        ("\\.tar\\'" . tar-mode)
        ("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'" . archive-mode)
        ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'" . archive-mode)
        ("\\`/tmp/Re" . text-mode)
        ("/Message[0-9]*\\'" . text-mode)
        ("/drafts/[0-9]+\\'" . mh-letter-mode)
        ("\\.zone\\'" . zone-mode)
        ("\\`/tmp/fol/" . text-mode)
        ("\\.y\\'" . c-mode)
        ("\\.lex\\'" . c-mode)
        ("\\.oak\\'" . scheme-mode)
        ("\\.sgml?\\'" . sgml-mode)
        ("\\.xml\\'" . sgml-mode)
        ("\\.dtd\\'" . sgml-mode)
        ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
        ("\\.idl\\'" . c++-mode)
        ("[]>:/\\]\\..*emacs\\'" . emacs-lisp-mode)
        ("\\`\\..*emacs\\'" . emacs-lisp-mode)
        ("[:/]_emacs\\'" . emacs-lisp-mode)
        ("\\.ml\\'" . lisp-mode)))

