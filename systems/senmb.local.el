;;
;;  Local Macbook Settings
;;

(require 'tramp)
(setq tramp-default-method "ssh")

;;
;; Set Frame Size in a Window mode 
;; and adjust font and theme
(when (window-system)
  (set-frame-size (selected-frame) 125 40)
  (set-face-attribute 'default nil :family "Monaco" :height 130)
  ;; on Mac emacs typefonts look better in dark
  (load-theme 'base16-default t))

(defun ffy-adjust-faces ()
  "This one changes faces that come with tomorrow theme and look better on the Macbook DELL screen. Probably should go into a theme."
  (dolist (face-spec '((region (t (:background "#404040")))
                       (highlight (t (:background "slate gray")))))
     (face-spec-set (car face-spec)
                    (cdr face-spec))))

(add-hook 'after-init-hook 'ffy-adjust-faces)

;;; Spellchecker setup
(eval-after-load "ispell"
  '(progn
    (when (executable-find "hunspell")
      (setq ispell-program-name "hunspell"
            ispell-extra-args '("-d" "/opt/local/share/hunspell/en_US" "-i" "utf-8")))))
;;; adjust PATH in emacs to what it is in the terminal
(setenv "PATH"
        (with-temp-buffer
          (call-process (getenv "SHELL") nil (current-buffer) nil "--login" "-i" "-c" "echo \"$PATH\"")
          (car (split-string (buffer-string) "\n" t))))
(setq exec-path (split-string (getenv "PATH") path-separator t))