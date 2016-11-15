;;; Searching in files and in buffers

;;; TODO: make moccur windows work like occur
(use-package color-moccur
  :ensure t
  :commands isearch-moccur-all
  :bind ("M-s O" . moccur)
  :init
  (bind-key "M-s O" #'isearch-moccur-all isearch-mode-map))

(use-package occur
  :bind (:map occur-mode-map
	      ("n" . occur-next)
	      ("p" . occur-prev)))

;;; This is a custom version of the library that should be loaded from
;;; the git submodule
(use-package ag
  :init (progn
	  ;; since we use our custom `ag` package we need to load its deps
	  (use-package s :ensure t :defer t)
	  (use-package dash :ensure t :defer t))
  :config (setq ag-reuse-buffers t
                ag-highlight-search t))

(provide 'ffy-search)
