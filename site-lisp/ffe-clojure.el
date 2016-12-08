(use-package cider
  :after (paredit eldoc)
  :ensure t
  :commands (cider-jack-in)
  :config
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(provide 'ffe-clojure)
