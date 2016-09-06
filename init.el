(defconst *emacs-start-time* (current-time))

(defconst *dotfiles-dir*
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")

(defconst *elpa-dir*
  (file-name-as-directory (concat *dotfiles-dir* "elpa"))
  "Directory for ELPA packages")

(defconst *data-dir*
  (file-name-as-directory (concat *dotfiles-dir* "data"))
  "Directory for miscellaneous data, s.a. backups, histories and caches")

(defconst *backup-dir*
  (file-name-as-directory (concat *data-dir* "backups"))
  "Directory for backups")

(defconst *lisp-dir*
  (file-name-as-directory (concat *dotfiles-dir* "site-lisp"))
  "Directory for Emacs Extensions files")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	    Add `*lisp-dir*' paths recursively to `load-path'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create recursive function
(defun add-directory-to-path (dir)
  (add-to-list 'load-path dir)
  (dolist (entry (directory-files-and-attributes dir))
    (if (and (cadr entry) ; t for directory
             (not (member (car entry) '("." "..")))) ; we don't want to deal with . and ..
        (let ((new-directory (expand-file-name (car entry) dir)))
          (add-to-list 'load-path new-directory)
          (add-directory-to-path new-directory)))))
;;; add the directory tree
(add-directory-to-path *lisp-dir*)
;;; erase the function
(fmakunbound #'add-directory-to-path)

(setq package-user-dir *elpa-dir* 
      package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")))
(package-initialize)
;; the rest of the package installation is hinged on this one
(package-install 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			        Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-save-list-file-prefix (concat *data-dir* "auto-save-list/.saves-"))

(use-package saveplace
  :init
  (progn
    (if (fboundp 'save-place-mode)
	;; Emacs 25 has a proper mode for `save-place'
	(save-place-mode)
      (setq save-place t))
    (setq save-place-file (concat *data-dir* "places"))))
(use-package desktop
  :defer t
  :init (if (fboundp 'desktop-save-mode)
	    (desktop-save-mode)
	  (setq desktop-save t))
  :config
  (progn
    (setq desktop-dirname *data-dir*)
    (push *data-dir* desktop-path)))
(use-package recentf
  :commands (recentf-open-most-recent-file)
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
					   (recentf-mode)
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

;; Minibuffer history
(use-package savehist
  :init
  (progn
    (setq savehist-file (concat *data-dir* "history")
	  enable-recursive-minibuffers t ; Allow commands in minibuffers
	  history-length 1000
	  savehist-additional-variables '(mark-ring
					  global-mark-ring
					  search-ring
					  regexp-search-ring
					  extended-command-history)
	  savehist-autosave-interval 60)
    (savehist-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat *data-dir* "bookmarks")
	bookmark-save-flag 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			      UI changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-minor-mode 'menu-bar-mode nil)
(custom-set-minor-mode 'tool-bar-mode nil)
(custom-set-minor-mode 'scroll-bar-mode nil)

(when (display-graphic-p)
  (custom-set-minor-mode 'mouse-wheel-mode t)
  (custom-set-minor-mode 'blink-cursor-mode nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smex
  :ensure t
  :init
  (setq smex-save-file (concat *data-dir* ".smex-items")
        smex-history-length 20)
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   Load custom-vars File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   How long it took
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading Emacs...done (%.3fs)" elapsed))
