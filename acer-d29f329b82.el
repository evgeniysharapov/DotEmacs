;;
;;  Evgeniy Sharapov 
;;  this is a sppecifics config file for mom's laptop in Perm, Russia
;;

; maximize Emacs frame
(global-set-key [(control f11)]
                (make-interactive w32-send-sys-command #xf030 nil))
; restore original size of the Emacs frame
(global-set-key [(control shift f11)]
                (make-interactive w32-send-sys-command #xf120 nil))
; set font for the Windows
(set-face-attribute 'default nil :family "Consolas" :height 100)
