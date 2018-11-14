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
(unless package-archive-contents
  (package-refresh-contents))
(package-install (intern "use-package"))

(setq use-package-enable-imenu-support t)

;;; Utility Packages
(use-package s :ensure t :defer t)
(use-package dash :ensure t :defer t)
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

(bind-keys  :prefix-map ctl-x-w-map
           :prefix "C-x w"
           :prefix-docstring "Window operations map")

(let ((c-x-z (global-key-binding [(control x) ?z])))
  (global-unset-key [(control x) (control ?z)])
  (define-key ctl-x-map [(control ?z)] c-x-z))

(use-package ffe-ui :demand t)

(use-package ffe-files)

(use-package ffe-navigation)

(use-package ffe-search)

(use-package ffe-edit)

(use-package ffe-visibility)

(use-package ffe-buffers)

(use-package ffe-spell)

(use-package ffe-help)

(use-package ffe-ido)

;;; short response function instead of long one
(fset 'yes-or-no-p 'y-or-n-p)

(use-package calendar
  :config
  (setq diary-file (concat *data-dir* "diary")))

;;; Completion in buffer
(use-package company
  :ensure t
  :defer 3
  :diminish company-mode
  :init
  (setq company-echo-delay 0)
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

  :bind  (:map company-active-map
	       ("<tab>" . company-complete-selection)
	       ("TAB" . company-complete-selection)
               ("M-/" . company-complete-common)
               ;; return just enters new line 
               ("<return>" . nil)
               ("RET" . nil)
               :filter (company-explicit-action-p)
               ;; unless completion was explicitly called
               ("<return>" . company-complete-selection)
               ("RET" . company-complete-selection))
  :bind* (("C-M-i" . company-manual-begin)))

(use-package company-statistics
  :ensure t
  :commands (company-statistics-mode)
  :init (progn
	  (setq company-statistics-file (concat *data-dir* "company-statistics-cache.el"))
	  (add-hook 'company-mode-hook #'company-statistics-mode)))

;;; Expandable snippets
(use-package yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-reload-all)
  :defer 5
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (concat *data-dir* "snippets"))
  (yas-reload-all)
  (add-hook 'text-mode-hook #'yas-minor-mode-on)
  (add-hook 'prog-mode-hook #'yas-minor-mode-on)
  :bind (:map yas-minor-mode-map
              ("C-c y" . company-yasnippet)))

(use-package yasnippet-snippets
  :ensure t
  :init (yas-reload-all))


(use-package outshine
  :ensure t
  :init (defvar outline-minor-mode-prefix "\M-#")
  :config (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

;; (use-package outline
;;   :commands outline-minor-mode
;;   :init (setq outline-minor-mode-prefix (kbd "M-o"))
;;   :config (bind-keys :map outline-mode-prefix-map
;; 		     ;; motion
;; 		     ("u" . outline-up-heading)
;; 		     ("f" . outline-forward-same-level)
;; 		     ("b" . outline-backward-same-level)
;; 		     ("n" . outline-next-visible-heading)
;; 		     ("p" . outline-previous-visible-heading)
;; 		     ;; editing
;; 		     ("<" . outline-promote)
;; 		     (">" . outline-demote)
;; 		     ("^" . outline-move-subtree-up)
;; 		     ("v" . outline-move-subtree-down)
;; 		     ("C-SPC" . outline-mark-subtree)))


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
    (setq savehist-file (concat *data-dir* "history"))
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

(use-package idle-highlight-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-mode
  :pin melpa-stable
  :init (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

(use-package ffe-lisp)

(use-package ffe-clojure)

(use-package ffe-c
  :commands ffe-c-mode-hook
  :init (add-hook 'c-mode-common-hook #'ffe-c-mode-hook))

(use-package ffe-go)

(use-package ffe-javascript
  :after projectile)			; because we customize projectile for javascript

(use-package ffe-python)

(use-package ffe-rust)

(use-package groovy-mode
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			  Misc. File Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ffe-json)

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

  
  (defun markdown-enter-key+ ()
    "Modification of enter key that just jumps onto the next line if ENTER key is pressed while point is on header"
    (interactive)
    (if (thing-at-point-looking-at markdown-regex-header)
	(let ((next-line-add-newlines t))
          (next-line))
      (markdown-enter-key)))

  (bind-key "RET" #'markdown-enter-key+ markdown-mode-map)
  
  :init (setq flyspell-generic-check-word-predicate
	      'flyspell-markdown-check-word-predicate))

(use-package less-css-mode
  :ensure t)

(use-package sh-script
  :mode (("\\.zsh" . sh-mode)))
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
	  (use-package json-rpc :ensure t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		    Loading System Specific Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when-let ((local-settings (concat *dotfiles-dir* (system-name) ".el"))
	   (exists (file-exists-p local-settings)))
  (load local-settings))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   Load custom-vars File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

;;; Start server-mode if we are not in the daemon mode
(use-package server
  :config
  (progn 
    (unless (or (daemonp) (server-running-p))
      (server-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   How long it took
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading Emacs...done (%.3fs)" elapsed))
(put 'narrow-to-region 'disabled nil)
