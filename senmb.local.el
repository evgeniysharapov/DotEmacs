;;
;;  Local Macbook Settings
;;

(require 'tramp)
(setq tramp-default-method "ssh")

;;
;; Set Frame Size in a Window mode 
;;
;; TODO: justify for the face font size 
(if (window-system)
    (set-frame-size (selected-frame) 165 42))
