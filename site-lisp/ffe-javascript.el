;;; Javascript Configuration
;;
;; Node packages expected to be installed globally:
;; 1. tern
;; 2. mocha

(use-package js2-mode
  :defer t
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'imenu-add-menubar-index))

(use-package tern
  :commands (tern-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :after company
  :ensure t
  :init (progn
          (defun ffe-js2-mode-company-hook ()
            (ffe-add-company-backends 'company-tern 'company-semantic))
	  
	  (add-hook 'js2-mode-hook #'ffe-js2-mode-company-hook)))

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

;;; customize projectile
(projectile-register-project-type 'npm '("package.json") nil "npm test" nil ".spec")

(provide 'ffe-javascript)
