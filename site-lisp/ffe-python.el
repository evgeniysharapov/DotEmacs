;;; Python Configuration
;;;
;;; make sure to install pyreadline on Windows.
;;;
(use-package python
  :defer t
  :diminish (python-mode . "Py")
  :commands python-mode
  :init
  (progn
    (setq python-shell-interpreter "python"
          python-shell-prompt-block-regexp "\\s-*\\.\\.\\."))
  :config
  (progn
    (add-hook 'python-mode-hook #'eldoc-mode))
  :bind (:map python-mode-map
	      ;; python-eldoc-at-point is not really useful, instead
	      ;; use it for sending file to python shell
	      ("C-c C-f" . python-shell-send-file)
	      ("C-M-f" . python-nav-forward-sexp)
	      ;; moves between syntactic blocks (if,for,while,def,..)
	      ("C-M-b" . python-nav-backward-sexp)
	      ("M-}" . python-nav-forward-block)
	      ("M-{" . python-nav-backward-block)
	      ;; In python this looks like next line skipping comments and multi-line strings
	      ("M-e" . python-nav-forward-statement)
	      ("M-a" . python-nav-backward-statement)))
;;
;; If anaconda produces error like
;; TypeError: __init__() got an unexpected keyword argument 'environment'
;; see https://github.com/proofit404/anaconda-mode/issues/296
;; most likely issue is jedi < 0.12, check via `pip list`
;; install up to date version via `pip install -U jedi` and error goes away
(use-package anaconda-mode
  :defer t
  :ensure t
  :diminish (anaconda-mode . "Ana")
  :init (progn
	  (setq anaconda-mode-installation-directory (concat *data-dir* "anaconda-mode"))
          (add-hook 'python-mode-hook #'anaconda-mode)
          (add-hook 'python-mode-hook #'anaconda-eldoc-mode))
  :config (progn
            ;; This one will prevent annoying window from popping up
            ;; see https://github.com/proofit404/anaconda-mode/issues/164
            ;; and https://github.com/syl20bnr/spacemacs/issues/3772
            (remove-hook 'anaconda-mode-response-read-fail-hook
                         'anaconda-mode-show-unreadable-response))
  :bind (:map anaconda-mode-map
	      ;; make key bindings more traditional
	      ("M-," . anaconda-mode-go-back)
	      ("M-*" . anaconda-mode-find-assignments)
	      ("C-c C-d" . anaconda-mode-show-doc)))


(use-package company-anaconda
  :defer t
  :ensure t
  :init (with-eval-after-load 'company
          (add-hook 'python-mode-hook (lambda () (ffe-add-company-backends 'company-anaconda)))))


(provide 'ffe-python)
