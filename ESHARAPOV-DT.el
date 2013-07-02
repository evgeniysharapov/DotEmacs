;;
;; Stone River Specific Settings File
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
;;; So, use the following Autohotkey script to remap Win+Arrow
;;     $#Right::
;;     IfWinActive ahk_class Emacs
;;     	  Send, ^z^w^{Right}
;;     return
;;     $#Left::
;;     IfWinActive ahk_class Emacs
;;        Send, ^z^w^{Left}
;;     return
;;     $#Up::
;;     IfWinActive ahk_class Emacs
;;     	  Send, ^z^w^{Up}
;;     return
;;     $#Down::
;;     IfWinActive ahk_class Emacs
;;     	  Send, ^z^w^{Down}
;;     return
;;; and put windmove operations onto some very useles key chords
;;; The only problem is describe-key won't tell us about Win-Arrow
(define-key ctrl-z-map [(control ?w) (control left)] 'windmove-left)
(define-key ctrl-z-map [(control ?w) (control right)] 'windmove-right)
(define-key ctrl-z-map [(control ?w) (control up)] 'windmove-up)
(define-key ctrl-z-map [(control ?w) (control down)] 'windmove-down)

(eval-after-load "ispell"
  '(progn
     (setq ispell-program-name "hunspell"
           ispell-extra-args '("-d" "C:\\Users\\esharapov\\App\\bin\\en_US" "-i" "utf-8"))))

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
