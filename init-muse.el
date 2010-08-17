;;  
;;  Emacs Configuration
;;  Setup for a Muse Mode 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)

;; do the setup properly
;; create a lo
(add-hook 'muse-mode-hook 'footnote-mode)
(add-hook 'muse-mode-hood
          (lambda ()
            (turn-on-auto-fill)))

(provide 'init-muse)
