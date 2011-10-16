;;
;;  Specific settings for different OS
;;  point being that I have several computers running the same OS
;;  and it is quite tedious to copy/paste the same settings between
;;  files. 
;;
;;  Evgeniy Sharapov 
;;  evgeniy.sharapov@gmail.com
;;

;; settings for Windows 
(when (equal system-type 'windows-nt)
                                        ; maximize Emacs frame
  (global-set-key [(control f11)]
                  (make-interactive w32-send-sys-command #xf030 nil))
                                        ; restore original size of the Emacs frame
  (global-set-key [(control shift f11)]
                  (make-interactive w32-send-sys-command #xf120 nil))
                                        ; set font for the Windows
  (set-face-attribute 'default nil :family "Consolas" :height 100)
  )
