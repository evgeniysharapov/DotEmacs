;; Configuration of IDO

(use-package ido
  :init (setq ido-save-directory-list-file (concat *data-dir* ".ido.last"))
  :config
  (ido-mode t))

(provide 'ffy-ido)
