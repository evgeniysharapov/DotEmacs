;; Configuration of IDO Completion system
(defvar *data-dir*)
(defvar recentf-list)

(use-package ido
  :defer 10
  :init (progn
	  (setq ido-save-directory-list-file (concat *data-dir* ".ido.last"))
	  (use-package ido-ubiquitous
	    :ensure t
	    :commands ido-ubiquitous-mode)
	  (use-package flx-ido
	    :ensure t
	    :commands flx-ido-mode))
  :config (progn
	    (ido-mode t)
	    (ido-everywhere t)
	    (ido-ubiquitous-mode t)
	    (flx-ido-mode t)
	    ;; use flx-ido highlights
	    (setq ido-enable-flex-matching t)
	    ;(setq ido-use-faces nil)
	    
	    
	    (setq-default org-completion-use-ido t)

	    (defun ido-find-recent-file ()
	      "Use `ido-completing-read' to \\[find-file] a recent file"
	      (interactive)
	      (unless recentf-mode
		(recentf-mode t))
	      (if (find-file (ido-completing-read+ "Find recent file: " recentf-list))
		  (message "Opening file...")
		(message "Aborting")))

	    (fset 'find-recent-file 'ido-find-recent-file)))

(provide 'ffe-ido)
