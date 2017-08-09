;;; Python Configuration
;;;
;;; make sure to install pyreadline on Windows.
;;;
(use-package python
  :defer t
  :commands python-mode
  :config
  (progn
    ;; (when (executable-find "ipython")
    ;;   (setq python-shell-interpreter "ipython"
    ;;                                     ;python-shell-prompt-input-regexps "In \\[[0-9]+\\]: "
    ;;                                     ;python-shell-prompt-output-regexps "Out\\[[0-9]+\\]: "
    ;;         ;;python-shell-interpreter-args "--simple-prompt --pprint"
    ;;         ;;python-shell-prompt-block-regexp "\\s-*\\.\\.\\."
    ;;         ;;python-shell-completion-setup-code  "from IPython.core.completerlib import module_completion"
    ;;         ;;python-shell-completion-string-code  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
    ;;         )
    ;;   ;; set the PAGER to 'less', otherwise IPython keeps failing
    ;;   (setenv "PAGER" "less"))
    
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

(use-package anaconda-mode
  :defer t
  :ensure t
  :diminish "Ana"
  :init (progn
	  (setq anaconda-mode-installation-directory (concat *data-dir* "anaconda-mode"))
          (add-hook 'python-mode-hook #'anaconda-mode)
          (add-hook 'python-mode-hook #'anaconda-eldoc-mode))
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
