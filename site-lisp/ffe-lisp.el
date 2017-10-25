;;; This is a lisp based programming language configuration
;;; mostly Emacs lisp and IELM and such
(defconst *lisp-mode-hooks* '(emacs-lisp-mode-hook
			      ielm-mode-hook
			      lisp-mode-hook
			      lisp-interaction-mode-hook))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :config (progn (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
		 ;; we use M-s for searching stuff
		 (unbind-key "M-s" paredit-mode-map)
		 ;; bind splice onto M-k since we shouldn't use it in lisp
		 ;; mode anyway
		 (bind-key "M-k" #'paredit-splice-sexp paredit-mode-map)))

(use-package elisp-slime-nav
  :ensure t
  :commands elisp-slime-nav-mode
  :diminish elisp-slime-nav-mode)

(dolist (mode-hook *lisp-mode-hooks*)
  (add-hook mode-hook #'paredit-mode)
  (add-hook mode-hook #'elisp-slime-nav-mode)
  (add-hook mode-hook #'eldoc-mode))

;; (global-set-key [remap eval-expression] 'pp-eval-expression)
;; (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(bind-key "C-c C-c" #'eval-buffer emacs-lisp-mode-map)

;;; turn off checkdoc for my configuration files
(defun ffe-disable-elisp-checkdoc-in-configuration-files ()
            (if (and (eq major-mode 'emacs-lisp-mode)  ; if it is elisp
		     (or
		      (string-equal user-init-file (buffer-file-name))	; or init.el file
		      (string-equal custom-file (buffer-file-name)) ; customization file
		      (and 		; configuration modules
		       (string-match (or (file-name-directory (or  (buffer-file-name) "")) "") *lisp-dir*)
		       (string-match "^ffe-" (or (file-name-nondirectory (or  (buffer-file-name) "")) "")))))
                (flycheck-disable-checker 'emacs-lisp-checkdoc)))

(add-hook 'emacs-lisp-mode-hook #'ffe-disable-elisp-checkdoc-in-configuration-files)

(defun ffe-ielm ()
  "Starts IELM or switches to existing one in the new window and sets working buffer of IELM to the current buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (if (get-buffer "*ielm*")
        (switch-to-buffer-other-window "*ielm*")
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm)))
    (ielm-change-working-buffer buf)))

(bind-key "C-M-:" #'ffe-ielm)


(provide 'ffe-lisp)
