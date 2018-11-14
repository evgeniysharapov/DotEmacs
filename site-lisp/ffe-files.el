;;;			        Files
(defvar *data-dir*)

(setq auto-save-list-file-prefix (concat *data-dir* "auto-save-list/.saves-"))

(use-package saveplace
  :init
  (setq save-place-file (concat *data-dir* "places")))

(use-package desktop
  :defer t
  :config
  (progn
    (setq desktop-dirname *data-dir*)
    (push *data-dir* desktop-path)))

(use-package recentf
  :commands (recentf-mode recentf-open-most-recent-file)
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
					   (recentf-mode t)
					   (recentf-track-opened-file))))
    (setq recentf-save-file (concat *data-dir* ".recentf")
	  recentf-max-saved-items 1000
	  recentf-auto-cleanup 'never
	  recentf-auto-save-timer (run-with-idle-timer 600 t
						       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude
		 (expand-file-name *data-dir*))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package find-file-in-project
  :ensure t
  :commands find-file-in-project
  :config (setq ffip-prefer-ido-mode t
                ffip-match-path-instead-of-filename t))

(use-package projectile
  :ensure t
  :diminish "Prj"
  :commands (projectile-mode projectile-register-project-type)
  :init 
  (setq projectile-keymap-prefix (kbd "C-z p")
	projectile-cache-file (expand-file-name "projectile.cache" *data-dir*)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" *data-dir*)
	projectile-switch-project-action (lambda () (dired (projectile-project-root))))
  :config
  (progn
    (projectile-mode t)))

(defun find-recent-file ())

(bind-key  "R" 'recentf-open-most-recent-file ctl-x-f-map)
(bind-key  "f" 'find-file-in-project          ctl-x-f-map)
(bind-key  "r" 'find-recent-file              ctl-x-f-map)
(bind-key  "." 'find-file-at-point            ctl-x-f-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Dired	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :init (add-hook 'dired-mode-hook #'hl-line-mode)
  :config
  (unbind-key "M-s f" dired-mode-map)
  (unbind-key "M-s a" dired-mode-map))

(put 'dired-find-alternate-file 'disabled nil)

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(provide 'ffe-files)
