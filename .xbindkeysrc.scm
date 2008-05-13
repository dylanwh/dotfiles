;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start of xbindkeys configuration ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This configuration is guile based.
; any functions that work in guile will work here.
; see EXTRA FUNCTIONS:

; Version: 1.7.1

; If you edit this file, do not forget to uncomment any lines
; that you change.
; The semicolon(;) symbol may be used anywhere for comments.

; To specify a key, you can use 'xbindkeys --key' or
; 'xbindkeys --multikey' and put one of the two lines in this file.

; A list of keys is in /usr/include/X11/keysym.h and in
; /usr/include/X11/keysymdef.h
; The XK_ is not needed.

; List of modifier:
;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).


; The release modifier is not a standard X modifier, but you can
; use it if you want to catch release instead of press events

; By defaults, xbindkeys does not pay attention to modifiers
; NumLock, CapsLock and ScrollLock.
; Uncomment the lines below if you want to use them.
; To dissable them, call the functions with #f
;EXTRA FUNCTIONS:
;(set-numlock! #t)
;(set-scrolllock! #t)
;(set-capslock! #t)
;(xbindkey key "foo-bar-command [args]")
;(xbindkey '(modifier* key) "foo-bar-command [args]")
;that is, xbindkey can take a list of modifiers ended by a key
;                  or it can just take a plain key.

(define HOST (getenv "HOST"))

;(xbindkey '(mod4 "p")     "exec pmenu")
;(xbindkey '(mod4 "o")     "exec pmenu -e")
;(xbindkey '(mod4 "grave") "exec pterm")
;(xbindkey '(mod4 "u")     "exec pterm -e irssi-urlview")

(if (not (equal? HOST "saga"))
  (begin
  	(xbindkey '(mod4 "Prior") "exec osdc vol up 10")
	(xbindkey '(mod4 "Next") "exec osdc vol down 10")
	(xbindkey '(mod4 "Pause") "exec osdc vol mute")
	(xbindkey '("m:0x1040" "b:5") "exec osdc vol down 10")
	(xbindkey '("m:0x840" "b:4") "exec osdc vol up 10")
	(xbindkey '("XF86AudioRaiseVolume") "exec osdc vol up 3")
	(xbindkey '("XF86AudioLowerVolume") "exec osdc vol down 3")
	(xbindkey '("XF86AudioMute") "exec osdc vol mute")
	(xbindkey '("XF86AudioPrev") "exec osdc mpc prev")
	(xbindkey '("XF86AudioNext") "exec osdc mpc next")
	(xbindkey '("XF86AudioStop") "exec osdc mpc stop")
	(xbindkey '("XF86AudioPlay") "exec osdc mpc toggle")))

