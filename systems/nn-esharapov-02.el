;;
;; Infusion laptop Settings File
;; We know that it's a Windows Machine
;; Evgeniy Sharapov, 2013
;;

;;; Windows specific stuff
(when (equal window-system 'w32)
    (setq
       w32-pass-lwindow-to-system nil
       w32-lwindow-modifier 'super
       w32-pass-rwindow-to-system nil
       w32-rwindow-modifier 'super
       w32-pass-apps-to-system nil
       w32-apps-modifier 'hyper
       w32-pass-alt-to-system nil
       w32-scroll-lock-modifier nil))

;;; Windows machine doesn't let us use Win + arrow keys
;;; So, use the following Autohotkey script to remap Win+Arrow for
;;; Emacs application and circumvent Windows specific keys 

;; $#Right::
;; IfWinActive ahk_class Emacs
;; 	Send, ^!+{NumpadRight}
;; return
;; $#Left::
;; IfWinActive ahk_class Emacs
;; 	Send, ^!+{NumpadLeft}
;; return
;; $#Up::
;; IfWinActive ahk_class Emacs
;; 	Send, ^!+{NumpadUp}
;; return
;; $#Down::
;; IfWinActive ahk_class Emacs
;; 	Send, ^!+{NumpadDown}
;; return
;;

;;; Now, using `key-translation-map' we are translating those key
;;; combinations sent by Autohotkey back into Win-Arrow (assuming that
;;; Win key is mapped to Super (see `w32-lwindow-modifier' and `w32-rwindow-modifier'))

(dolist (direction-symbol '(left right up down))
  (let* ((direction (symbol-name direction-symbol))
         (windmove-command (intern  (concat  "windmove-"  direction)))
         (keypress-numlock-off (concat  "<C-M-S-kp-"  direction ">"))
         (keypress-numlock-on (concat  "<C-M-kp-" direction ">"))
         (super-direction-keypress (concat  "<s-" direction ">")))
    (define-key key-translation-map (kbd  keypress-numlock-off) (kbd super-direction-keypress) )
    (define-key key-translation-map (kbd  keypress-numlock-on) (kbd super-direction-keypress))))

(eval-after-load "ispell"
  '(progn
     (setq ispell-program-name "hunspell"
           ispell-extra-args '("-d" "C:\\App\\hunspell\\bin\\en_US" "-i" "utf-8"))))

;;; Org-mode hackery
(setq org-clock-clocktable-default-properties
      (list :maxlevel 4
            :scope 'file
            :step 'day
            :block 'week
            :narrow '80!
            :link t
            :properties '("OpenAir" "Support")
            :inherit-props t))

;;; Font properties on my work Windows machine
(set-face-attribute 'default nil :family "Consolas" :height 100)

;;; loading up color theme
;(load-theme 'tango-plus t) ; need to add it to the 'custom-theme-load-path
(load "tango-plus-theme.el")
