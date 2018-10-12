;;; Searching in files and in buffers
;;;
;;; Consistent keys in Grep and Occur modes unifying `moccur-mode-map', `grep-mode-map', `occur-mode-map'
;;; 
;;; p - previous select
;;; n - next select
;;; M-p - prev without  selection
;;; M-n - next  without selection
;;; M-{, { - prev file (buffer)
;;; M-}, } - next file (buffer)

;;; TODO: make moccur windows work like occur
(use-package color-moccur
  :ensure t
  :commands isearch-moccur-all
  :bind (("M-s M-o" . moccur)
	 :map isearch-mode-map
	 ("M-s M-o" . isearch-moccur-all)
	 :map moccur-mode-map
	 ("M-p" . moccur-prev)
	 ("M-n" . moccur-next)
	 ("M-{" . moccur-prev-file)
	 ("M-}" . moccur-next-file)
	 ("{" . moccur-prev-file)
	 ("}" . moccur-next-file)))

(defun occur-symbol-at-point ()
  "Calls `occur' with a symbol at point"
  (interactive)
  (let ((s (symbol-name (symbol-at-point))))
    (push s regexp-history)
    (occur s)))

(defun occur-prev-buffer ()
  "moves to the occur entry in the previous buffer"
  (interactive)
  (if (re-search-backward "^[0-9]+ matches? in buffer: " nil t 2)
      (occur-next 1)
    (goto-char (point-max))
    (if (re-search-backward "^[0-9]+ matches? in buffer: " nil t)
	(occur-next 1))))

(defun occur-next-buffer ()
  "Moves to the occur entry in the next buffer"
  (interactive)
  (if (re-search-forward "^[0-9]+ matches? in buffer: " nil t)
      (occur-next 1)
    (goto-char (point-min))
    (occur-next 1)))

(defun occur-prev-error ()
  "Goes to the previous occur entry"
  (interactive)
  (occur-next-error -1))

(defun occur-prev-noselect ()
  "Pops up buffer location for the occur but stays in *Occur* buffer"
  (interactive)
  (occur-prev-error)
  (other-window 1))

(defun occur-next-noselect ()
  "Pops up buffer location for the occur but stays in *Occur* buffer"
  (interactive)
  (occur-next-error 1)
  (other-window 1))

(use-package replace
  :bind (("M-s O" . multi-occur)
	 ("M-s m" . multi-occur-in-matching-buffers)
	 ("M-s M-s" . occur-symbol-at-point)
	 :map occur-mode-map
	 ("M-n" . occur-next-error)
	 ("M-p" . occur-prev-error)
	 ("{" . occur-prev-buffer)
	 ("M-{" . occur-prev-buffer)
	 ("}" . occur-next-buffer)
	 ("M-}" . occur-next-buffer)
	 ("n" . occur-next-noselect)
	 ("p" . occur-prev-noselect)))

(use-package wgrep
  :ensure t)

;; This is a custom version of the library that should be loaded from
;; the git submodule
(use-package ag
  :defines (ag-reuse-buffers ag-highlight-search)
  :init (progn
	  ;; since we use our custom `ag` package we need to load its deps
	  (use-package s :ensure t :defer t)
	  (use-package dash :ensure t :defer t))
  :config (progn  (setq ag-reuse-buffers t
			ag-highlight-search t)
		  (use-package wgrep-ag :ensure t :defer t))
  :bind (("M-s a" . ag)
	 ("M-s p" . ag-project)))

(use-package grep
  :commands (grep rgrep grep-find)
  :bind (("M-s g" . grep)
	 ("M-s f" . grep-find)
	 ("M-s r" . rgrep)))

;; These are handy keys when you are navigating a buffer using isearch
(use-package isearch 
  :bind (:map isearch-mode-map
              ("<up>" . isearch-repeat-backward)
              ("<down>" . isearch-repeat-forward)))

(use-package visual-regex)
(provide 'ffe-search)
