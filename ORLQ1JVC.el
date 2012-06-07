;;
;; Siemens Specific Emacs Configuration.
;; We know that it's a Windows Machine 
;; Evgeniy Sharapov, 2010 
;;

;;
;; ORG-Mode Special Tweaks
;;
;; Adding functionality to integrate with Outlook
(require 'org-outlook)
(when (fboundp 'w32-short-file-name)
  (setq org-outlook-location (w32-short-file-name "c:/Program Files/Microsoft Office/OFFICE11/OUTLOOK.exe")))

;;
;; Spell checking for Emacs usin Hunspell
;;
(eval-after-load "ispell"
    (progn
      (setq ispell-dictionary "american"
	    ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
	    ispell-silently-savep t
            ispell-aspell-dictionary-alist
            '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "C:/Apps/Hunspell/en_US") nil utf-8)))))
(setq-default ispell-program-name "hunspell")
