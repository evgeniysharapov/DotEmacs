;;; Navigation in the buffer
(defvar *data-dir*)

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat *data-dir* "bookmarks")
	bookmark-save-flag 1))

(use-package ace-jump-mode
  :ensure t
  :bind (:map goto-map
	 ("j" . ace-jump-mode)))

(use-package imenu
  :commands (imenu)
  :init (progn
	  (use-package imenu+ :defer t)
	  (use-package imenu-list :ensure t :commands (imenu-list)))
  :bind (:map search-map
         ("i" . imenu)
	 ("I" . imenu-list)))

(provide 'ffe-navigation)
