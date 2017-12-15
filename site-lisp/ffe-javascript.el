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
  (progn
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook #'imenu-add-menubar-index)
    (add-hook 'js2-mode-hook #'idle-highlight-mode)
    (add-hook 'js2-mode-hook #'electric-pair-mode)
    (add-hook 'js2-mode-hook #'hs-minor-mode)))

(use-package tern
  :commands (tern-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

;;; We can't quite manipulate local mode keymap from within a hook, so we do it in EAL form
(eval-after-load "tern"
  '(progn
     ;; clear C-c C-r and C-c C-d from tern-keymap
     (define-key tern-mode-keymap (kbd  "C-c C-r") nil)
     (define-key tern-mode-keymap (kbd "C-c C-d") nil)
     (bind-keys :map tern-mode-keymap
     		("C-c C-r r" . tern-rename-variable)
     		("C-c C-d C-d" . tern-get-docs)
     		("C-c C-d d" . tern-get-type))))


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
              ("C-c t ." . mocha-test-at-point)
              ("C-c t f" . mocha-test-file)
              ("C-c t p" . mocha-test-project)))

(use-package js-comint
  :ensure t
  :commands (run-js))

;;; customize projectile
(projectile-register-project-type 'npm '("package.json") nil "npm test" nil ".spec")

(provide 'ffe-javascript)
