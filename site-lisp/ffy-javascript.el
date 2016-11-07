;;; Javascript Configuration

(use-package js2-mode
  :defer t
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'imenu-add-menubar-index)
  
  :config
  (setq js2-mode-show-parse-errors nil  ; disable parser errors
        js2-mode-show-strict-warnings nil ; disable strict warnings.
        js2-highlight-level 3   ; highlight ECMA built-ins
        ))

(use-package tern
  :commands (tern-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :after company
  :ensure t
  :init (progn
          (defun ffy-js2-mode-company-hook ()
            (ffy-add-company-backends 'company-tern 'company-semantic))
	  
	  (add-hook 'js2-mode-hook #'ffy-js2-mode-company-hook)))

(use-package mocha
  :commands (mocha-test-at-point mocha-test-file mocha-test-project)
  :ensure t
  :pin melpa-stable
  :bind (:map js2-mode-map
              ("C-c C-c ." . mocha-test-at-point)
              ("C-c C-c f" . mocha-test-file)
              ("C-c C-c p" . mocha-test-project)))

(use-package js-comint
  :ensure t
  :commands (run-js))

(provide 'ffy-javascript)
