;;; TeX Settings
(use-package tex-site                   ; AucTeX initialization
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ; parse documents for auto completion
        TeX-auto-save  t                ; parse on save
        ))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

        
(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t)

(use-package bibtex                     ; BibTeX editing
  :ensure auctex
  :defer t)

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'TeX-mode-hook #'reftex-mode))

;;; Metapost Extra
(use-package meta-mode
  :defer t)

;;;; Preview Metapost Buffer via MPtoPDF


;;; ConTexT Specifics
(use-package context
  :defer t
  :mode (("\\.mkiv\\'" . context-mode)
         ("\\.mkii\\'" . context-mode)))

;; The layout of the ConTeXt installation base is well-defined


(provide 'ffe-tex)
