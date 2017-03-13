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
(setq use-package-enable-imenu-support t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		     Keymap and keys organization 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ctl-z-map)
(define-prefix-command 'ctl-z-map)
(let ((c-z (global-key-binding [(control ?z)])))
  (global-unset-key [(control ?z)])
  (bind-key "C-z" 'ctl-z-map)
  (bind-key "C-z C-z" c-z))

(bind-keys :prefix-map ctl-x-f-map
           :prefix "C-x f"
           :prefix-docstring "File operations map")

(bind-keys :prefix-map ctl-x-t-map
           :prefix "C-x t"
           :prefix-docstring "Toggle map")

(bind-keys :prefix-map ctl-x-w-map
           :prefix "C-x w"
           :prefix-docstring "Window operations map")

(let ((c-x-z (global-key-binding [(control x) ?z])))
  (global-unset-key [(control x) (control ?z)])
  (define-key ctl-x-map [(control ?z)] c-x-z))


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
  :load-path "~/.emacs.d/site-lisp/projectile"
  :defer 5
  :diminish projectile-mode
  :commands (projectile-global-mode projectile-require-project-type)
  :init 
  (setq projectile-keymap-prefix (kbd "C-z p")
	projectile-cache-file (expand-file-name "projectile.cache" *data-dir*)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" *data-dir*)
        projectile-sort-order 'recentf
        projectile-indexing-method 'alien
	projectile-switch-project-action (lambda () (dired (projectile-project-root))))
  :config
  (projectile-global-mode t))

(defun find-recent-file ())

(bind-key  "R" 'recentf-open-most-recent-file ctl-x-f-map)
(bind-key  "f" 'find-file-in-project          ctl-x-f-map)
(bind-key  "r" 'find-recent-file              ctl-x-f-map)
(bind-key  "." 'find-file-at-point            ctl-x-f-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Dired	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat *data-dir* "bookmarks")
	bookmark-save-flag 1))

(use-package ace-jump-mode
  :ensure t
  :bind (:map goto-map
	 ("j" . ace-jump-mode)))

(use-package imenu
  :commands (imenu)
  :init (progn
	  (use-package imenu+ :ensure t :defer t)
	  (use-package imenu-list :ensure t :commands (imenu-list)))
  :bind (("M-s i" . imenu)
	 ("M-s I" . imenu-list)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package browse-kill-ring+
  :ensure t
  :defer 10
  :commands browse-kill-ring
  :bind (("C-x C-y" . browse-kill-ring)))

(use-package ffe-search)

(use-package hide-lines
  :defer t
  :bind (:map ctl-z-map
	      ("/" . hide-lines)))

(use-package ffe-ui)

(use-package windmove
  :ensure t
  :defer t
  :bind (:map ctl-x-w-map
              ("<left>" . windmove-left)
              ("h" . windmove-left)
              ("<right>" . windmove-right)
              ("l" . windmove-right)
              ("<up>" . windmove-up)
              ("j" . windmove-up)
              ("<down>" . windmove-down)
              ("k" . windmove-down)))

(use-package ace-window
  :ensure t
  :pin melpa-stable
  :bind ("C-x o" . ace-window))

(use-package ffe-help)

(use-package ffe-ido)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init (progn
          (defface ibuffer-custom-deletion-face
	    '((t (:inherit error :strike-through t :underline nil)))
	    "Buffers to be deleted")
          (defface ibuffer-custom-marked-face
	    '((t (:inherit warning :inverse-video t :underline nil)))
	    "Marked buffers")
          (setq ibuffer-deletion-face 'ibuffer-custom-deletion-face
                ibuffer-marked-face 'ibuffer-custom-marked-face)
          ;; auto updateable ibuffer
          (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)))

(bind-key "C-x K" #'kill-this-buffer)

;;; short response function instead of long one
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   General Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package misc
  :commands (zap-up-to-char)
  :bind (("M-z" . zap-up-to-char)))
(bind-key "M-Z" 'zap-to-char)
;; zapping back is done via negative argument C-- or M--

(use-package ffe-spell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :commands magit-status
  :bind (:map ctl-z-map
	      ("g" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package monky
  :ensure t
  :commands monky-status
  :bind (:map ctl-z-map
	      ("h" . monky-status)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			      Minibuffer
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

(defun ffe-auto-close-buffers ()
  "Closes buffers that should be closed after we done with minibuffer. Usually it is various completions buffers"
  (mapc #'(lambda (buf-name)
	   (let ((buffer (get-buffer buf-name)))
	     (if (buffer-live-p buffer)
		 (kill-buffer buffer)))) '("*Completions*" "*Ido Completions*")))

(add-hook 'minibuffer-exit-hook #'ffe-auto-close-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			  Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :init  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package hl-line-mode
  :commands hl-line-mode
  :init (add-hook 'prog-mode-hook #'hl-line-mode)
  :bind (:map ctl-x-t-map
	      ("h" . hl-line-mode)))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-mode
  :pin melpa-stable
  :init (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

(use-package company
  :ensure t
  :defer 3
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1
	company-tooltip-limit 20
	company-show-numbers t
	company-selection-wrap-around t
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t
	company-echo-delay 0)
  :config
  ;; default `company-backends'
  (setq company-backends '(company-capf
			   (company-dabbrev-code
			    company-gtags
			    company-etags
			    company-keywords)
			   company-files
			   company-dabbrev))

  (defun ffe-add-company-backends (&rest backends)
    "Adds BACKENDS to the beginning of the buffer-local version of `company-backends' list"
    (set (make-local-variable 'company-backends)
	 (append backends company-backends)))

  (global-company-mode 1)

  :bind (:map company-active-map
	      ("<right>" . company-complete-selection)))

(use-package company-statistics
  :ensure t
  :commands (company-statistics-mode)
  :init (progn
	  (setq company-statistics-file (concat *data-dir* "company-statistics-cache.el"))
	  (add-hook 'company-mode-hook #'company-statistics-mode)))

(use-package yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-global-mode yas-expand)
  :defer 5
  :ensure t
  :init (setq yas-snippet-dirs (list (concat *data-dir* "snippets")
				     'yas-installed-snippets-dir))
  :config
  (yas-reload-all)
  (add-hook 'text-mode-hook #'yas-minor-mode-on)
  (add-hook 'prog-mode-hook #'yas-minor-mode-on))

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

(use-package outline
  :commands outline-minor-mode
  :init (setq outline-minor-mode-prefix (kbd "M-o"))
  :config (bind-keys :map outline-mode-prefix-map
		     ;; motion
		     ("u" . outline-up-heading)
		     ("f" . outline-forward-same-level)
		     ("b" . outline-backward-same-level)
		     ("n" . outline-next-visible-heading)
		     ("p" . outline-previous-visible-heading)
		     ;; editing
		     ("<" . outline-promote)
		     (">" . outline-demote)
		     ("^" . outline-move-subtree-up)
		     ("v" . outline-move-subtree-down)
		     ("C-SPC" . outline-mark-subtree)))

(use-package elisp-slime-nav
  :ensure t
  :commands elisp-slime-nav-mode
  :diminish elisp-slime-nav-mode)

(defconst *lisp-mode-hooks* '(emacs-lisp-mode-hook
			      ielm-mode-hook
			      lisp-mode-hook
			      lisp-interaction-mode-hook))

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

(use-package ffe-clojure)

(use-package ffe-c
  :commands ffe-c-mode-hook
  :init (add-hook 'c-mode-common-hook #'ffe-c-mode-hook))

(use-package ffe-go)

(use-package ffe-javascript
  :after projectile)			; because we customize projectile for javascript

(use-package ffe-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			  Misc. File Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nginx-mode
  :ensure t
  :commands nginx-mode)

(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package markdown-mode  
  :ensure t
  :mode (("\\.md$" . markdown-mode))
  :config (add-hook 'markdown-mode-hook #'flyspell-mode)

  (defun flyspell-markdown-check-word-predicate ()
    "Used by `flyspell-mode' in Markdown documents to skip spell checking code blocks and inline code"
    (let ((face (get-text-property (-  (point) 1) 'face)))
      (not (memq face '(markdown-pre-face markdown-inline-code-face markdown-reference-face)))))
  
  :init (setq flyspell-generic-check-word-predicate
	      'flyspell-markdown-check-word-predicate))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			       Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ffe-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Docker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)
(use-package docker
  :ensure t
  :pin melpa-stable
  :commands (docker-containers)
  :init (progn
	  (use-package json-rpc :ensure t)
	  (when (eq  system-type 'windows-nt)
	    (setenv (or (getenv "DOCKER_HOST") "tcp://192.168.99.100:2376"))
	    (setenv (or (getenv "DOCKER_TLS_VERIFY") "1"))
	    (setenv (or (getenv "DOCKER_MACHINE_NAME") "default"))
	    (setenv (or (getenv "DOCKER_CERT_PATH") "C:\\Users\\esharapov\\.docker\\machine\\machines\\default")))))
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
