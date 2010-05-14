;;
;; Disney Specific Emacs Configuration.
;; We know that it's a Windows Machine 
;; Evgeniy Sharapov, 2010 
;;
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
; maximize Emacs frame
(global-set-key [(control f11)]
                (make-interactive w32-send-sys-command #xf030 nil))
  ; restore original size of the Emacs frame
(global-set-key [(control shift f11)]
                (make-interactive w32-send-sys-command #xf120 nil))
(set-face-attribute 'default nil :family "Consolas" :height 100)
