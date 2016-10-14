;;; Org mode configuration
(use-package org
  :mode (("\\.org$" . org-mode))
  :config (org-babel-do-load-languages
	   'org-babel-load-languages
	   '((ruby . t)
	     (python . t)
	     (emacs-lisp . t)
	     (latex . t)
	     (gnuplot . t)
	     (C . t)))
  (add-hook 'org-src-mode-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (flycheck-disable-checker 'emacs-lisp-checkdoc))))

  :bind (:map org-mode-map
              ("C-c k" . org-cut-subtree)
              ;;  Swap C-j and RET
              ([remap org-return-indent] . org-return)
              ([remap org-return] . org-return-indent)))


(provide 'ffy-org)
