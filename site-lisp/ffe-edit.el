;;;			   Editing

(use-package misc
  :commands (zap-up-to-char forward-to-word)
  ;; zapping back is done via negative argument C-- or M--
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)
         ("M-f" . forward-to-word)
         ("M-F" . forward-word)))
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :init 
  (use-package browse-kill-ring+
    :defer 10)
  :bind (("C-M-y" . browse-kill-ring)))


(provide 'ffe-edit)
