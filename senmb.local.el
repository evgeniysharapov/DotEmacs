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

;;; adjust PATH in emacs to what it is in the terminal
(setenv "PATH"
        (with-temp-buffer
          (call-process (getenv "SHELL") nil (current-buffer) nil "--login" "-i" "-c" "echo \"$PATH\"")
          (car (split-string (buffer-string) "\n" t))))
(setq exec-path (split-string (getenv "PATH") path-separator t))




