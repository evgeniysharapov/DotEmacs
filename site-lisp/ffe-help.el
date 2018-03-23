;;; Help/Info configuration
(use-package help-mode+)
(use-package help+)
(use-package help-fns+)
(bind-keys :map help-map
           ("C-b" . describe-personal-keybindings)
           ("C-k" . describe-key-briefly)
           ("C-c" . describe-char))

(provide 'ffe-help)
