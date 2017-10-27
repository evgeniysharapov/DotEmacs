;;;
;;; Configuration for Editing JSON files
;;;

(use-package json-mode
  :defer t
  :ensure t
  :init (progn
          (use-package json-navigator
           :ensure t)

         (use-package json-snatcher
           :commands (jsons-print-path)
           :ensure t)

         (add-hook 'json-mode-hook #'hs-minor-mode))
  
  :bind (:map json-mode-map
              ("C-c p" . jsons-print-path)))


(provide 'ffe-json)
