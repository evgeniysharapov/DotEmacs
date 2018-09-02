;;;
;; For Mac OS X install `aspell' using Homebrew
;;
;;     brew install aspell --with-all-langs
;; 
;;; Code:
;; In order for spelling to work correctly it expects DICTIONARY
;; environment variable to be set
;; Trick from http://www.emacswiki.org/emacs/InteractiveSpell
(setenv "DICTIONARY" "en_US")

(defvar *data-dir*)			; just  to quiet linter
(defvar ctl-z-map)

;; note that for aspell file has to have a first line
;; personal_ws-1.1 en 0
;;    https://blog.samat.org/2008/11/02/creating-your-own-personal-aspell-dictionary/
(defun ffe-personal-dictionary ()
  "Personal dictionary setup if file doesn't exist then create it"
  (let ((personal-dictionary-file (concat *data-dir* ".personal.dict")))
    (unless (file-exists-p personal-dictionary-file)
      (with-temp-file personal-dictionary-file t))
    personal-dictionary-file))

(use-package flyspell
  :commands (flyspell-buffer flyspell-mode flyspell-region)
  :config (setq flyspell-use-meta-tab nil
		flyspell-auto-correct-binding (kbd "C-M-;"))
  :bind (:map flyspell-mode-map
	      ("C-," . nil)
	      ("C-." . nil)
	      ("C-;" . nil)
	      ("C-c $" . nil)
	      ("C-$ n" . flyspell-goto-next-error)
	      ("C-$ b" . flyspell-buffer)
	      ("C-$ r" . flyspell-region)))

(use-package ispell
  :defer t
  :config (setq ispell-personal-dictionary (ffe-personal-dictionary)
		ispell-silently-savep t
		ispell-choices-win-default-height 3)
  :init (progn
	  ;; Aspell Specific
	  (when (executable-find "aspell")
	    (setq ispell-program-name "aspell"
		  ispell-extra-args '("--sug-mode=ultra")))
	  ;; Hunspell Specific
	  (when (executable-find "hunspell")
	    (setq ispell-program-name "hunspell"))))

(provide 'ffe-spell)
;;; ffe-spell ends here

