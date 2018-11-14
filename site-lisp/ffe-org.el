;;; Org mode configuration

(use-package ob-ipython
  :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config (org-babel-do-load-languages
	   'org-babel-load-languages
	   '((ipython . t)
             (ruby . t)
	     (python . t)
	     (emacs-lisp . t)
	     (latex . t)
	     (gnuplot . t)
	     (C . t)))
  (add-hook 'org-src-mode-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (flycheck-disable-checker 'emacs-lisp-checkdoc))))

  :bind (:map ctl-z-map
              ("a" . org-agenda)
              ("l" . org-store-link)
              ("b" . org-switchb)
         :map org-mode-map
              ("C-c k" . org-cut-subtree)
              ;;  Swap C-j and RET
              ([remap org-return-indent] . org-return)
              ([remap org-return] . org-return-indent)))

(use-package org-journal
  :ensure t)

(provide 'ffe-org)
