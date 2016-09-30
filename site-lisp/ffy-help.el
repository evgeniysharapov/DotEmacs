;;; Help/Info configuration
(use-package help-mode+ :ensure t)
(use-package help+	:ensure t)
(use-package help-fns+	:ensure t)
(bind-keys :map help-map
           ("C-b" . describe-personal-keybindings)
           ("C-k" . describe-key-briefly)
           ("C-c" . describe-char))

(provide 'ffy-help)
