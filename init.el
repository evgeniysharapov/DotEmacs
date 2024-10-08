;; -*- lexical-binding: t; -*-
;;; Macros and Functions
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;;; Constants and Paths
(defconst *emacs-start-time* (current-time))
;;;; Directories
(defconst *dotfiles-dir*
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")

(defconst *data-dir*
  (file-name-as-directory (concat *dotfiles-dir* "data"))
  "Directory for miscellaneous data, s.a. backups, histories and caches")

(defconst *backup-dir*
  (file-name-as-directory (concat *data-dir* "backups"))
  "Directory for backups")

(defconst *undo-dir*
  (file-name-as-directory (concat *data-dir* "undo"))
  "Directory for undo")

(defconst *elpa-dir*
  (file-name-as-directory (concat *data-dir* "elpa"))
  "Directory for ELPA packages")

(defconst *quelpa-dir*
  (file-name-as-directory (concat *data-dir* "quelpa"))
  "Directory for Quelpa packages")

(defconst *scripts-dir*
  (file-name-as-directory (concat *dotfiles-dir* "scripts"))
  "Directory with various OS scripts that are used by the Emacs")

(defconst *lisp-dir*
  (file-name-as-directory (concat *dotfiles-dir* "site-lisp"))
  "Directory for Emacs Extensions files")

;;;; Operating System and Environement

(defconst *is-linux* nil
  "Is t when we run it on Linux")

(defconst *is-wsl*
  (string-match-p "-[Mm]icrosoft" operating-system-release)
  "Is t if WSL/WSL2 and nil otherwise")

(defconst *is-macos*
  (eq 'darwin system-type)
  "Is t if we run on MacOS")

;; prefer loading newer packages
(setf load-prefer-newer t)

;;; Libraries and Packages 
;;;; Load Libraries Recursively
;; Add `*lisp-dir*' paths recursively to `load-path'
;; create recursive function
(defun add-directory-to-path (dir)
  (add-to-list 'load-path dir)
  (dolist (entry (directory-files-and-attributes dir))
    (if (and (cadr entry) ; t for directory
             (not (member (car entry) '("." "..")))) ; we don't want to deal with . and ..
        (let ((new-directory (expand-file-name (car entry) dir)))
          (add-to-list 'load-path new-directory)
          (add-directory-to-path new-directory)))))
;; add the directory tree
(add-directory-to-path *lisp-dir*)
;; erase the function
(fmakunbound #'add-directory-to-path)

;;;; Initialize `use-package' and friends
(require 'package)
(setf package-user-dir *elpa-dir*)

(customize-set-variable 'package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; this is important on Cisco Umbrella machine 
(setq package-check-signature nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  (use-package-enable-imenu-support t))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-dir *quelpa-dir* "Store quelpa packages here")
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

;; After installing `quelpa-use-package' we can use it to fetch
;; packages in `use-package'
;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa-use-package
  :ensure t)

;;;; Load custom-vars File
;; consider removing as much as possible from the custom file
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)


;;;; Useful Packages Loaded
(use-package s        :ensure t :defer t)
(use-package seq      :ensure t :defer t)
(use-package dash     :ensure t :defer t)
(use-package diminish :ensure t)
(use-package subr-x)

;;; Utility
;;;; History and Sessions
;; Backup Files
(use-package files
  :custom
  (backup-directory-alist `(("." . ,*backup-dir*)))
  (backup-by-copying-when-linked t)
  (backup-by-copying t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2))

;; Sessions
(use-package saveplace
  :custom
  (save-place-mode t)
  (save-place-file (concat *data-dir* "places")))

(setq auto-save-list-file-prefix (concat *data-dir* "auto-save-list/.saves-"))

(use-package desktop
  :defer t  
  :config
  (progn
    (setq desktop-dirname *data-dir*)
    (push *data-dir* desktop-path))
  :custom
  (desktop-globals-to-save
   '(desktop-missing-file-warning
     (search-ring . 50)
     (regexp-search-ring . 50)
     (regexp-history . 50)
     (grep-history . 50)
     register-alist file-name-history tags-file-name
     (shell-command-history . 50)
     (read-expressions-history . 50)
     (query-replace-history 0.5)
     (minibuffer-history . 50)
     (compile-history . 50)))
  (desktop-restore-eager 2)
  (desktop-restore-frames nil)
  (desktop-save t)
  (desktop-save-mode t))

;;;; Minibuffer

;; minibuffer history 
(use-package savehist
  :custom
  (savehist-file (concat *data-dir* "history"))
  :init
  (savehist-mode t))

;; Auto-closing of the utility buffers 
(defun ffe-auto-close-buffers ()
  "Closes buffers that should be closed after we done with minibuffer.
 Usually it is various completions buffers"
  (mapc #'(lambda (buf-name)
	   (let ((buffer (get-buffer buf-name)))
	     (if (buffer-live-p buffer)
		 (kill-buffer buffer)))) '("*Completions*" "*Ido Completions*")))
(add-hook 'minibuffer-exit-hook #'ffe-auto-close-buffers)
;; Use normal RET action
(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda () (unbind-key "RET" 'paredit-mode-map)))


;;;; Utility Functions
;;;;; UUID
;; From http://ergoemacs.org/emacs/elisp_generate_uuid.html
(defun ffe-uuid (&optional arg)
  "Returns GUID generated by an OS specific command.
With prefix of 4 (C-u) inserts uuid in a buffer."
  (interactive "P")  
  (let ((uuid
         (replace-regexp-in-string "\n$" ""
                                   (cond
                                    ((string-equal system-type "windows-nt")
                                     (shell-command-to-string "powershell.exe -Command [guid]::NewGuid().toString()"))
                                    ((string-equal system-type "darwin") ; Mac
                                     (shell-command-to-string "uuidgen"))
                                    ((string-equal system-type "gnu/linux")
                                     (shell-command-to-string "uuidgen"))))))
    (if arg
        (insert uuid)
      uuid)))

(defconst thing-at-point-uuid-regexp (rx bow (repeat 8 hex-digit) "-" (repeat 4 hex-digit) "-" (repeat 4 hex-digit) "-" (repeat 4 hex-digit) "-" (repeat 12 hex-digit) eow)
  "Regular Expression matching UUID")

(put 'uuid 'bounds-of-thing-at-point
     (lambda ()
       (when (thing-at-point-looking-at thing-at-point-uuid-regexp 36)
         (cons (match-beginning 0) (match-end 0)))))

(defun ffe-replace-uuid (&optional arg)
  "Replaces UUID with new generated UUID if point is on UUID"
  (interactive "P")
  (when (thing-at-point 'uuid)
    (let* ((bounds (bounds-of-thing-at-point 'uuid))
           (beg (car bounds))
           (end (cdr bounds)))
      (delete-region beg end)
      (ffe-uuid 4))))

;;;;; Filenames
;;;;;; Generating names 
(defun ffe-image-directory (fn &optional arg)
  "Generates directory name based on the given filename FN and optional argument ARG.

Examples:
(ffe-image-directory \"~/test/work.org\") => \"~/test/\"
(ffe-image-directory \"~/test/work.org\" 'file) => \"~/test/work/\"
(ffe-image-directory \"~/test/work.org\" 'img) => \"~/test/img/\"
"
  (let* ((parent-dir (file-name-directory fn)))
    (file-name-as-directory
     (cond
      ((eq arg 'file) (concat parent-dir (file-name-base fn)))
      ((eq arg 'img) (concat parent-dir "img"))
      (t parent-dir)))))

(defun ffe-image-filename (dir name)
  "Generates unique filename for the image to be stored in the image directory"
  (let ((ts (format-time-string "%Y%m%d_%H%M%S")))    
    (concat
     (file-name-as-directory dir)
     (make-temp-name (concat name "_" ts "_"))
     ".png")))

;;;;; Clipboard
(defun ffe-clipboard-to-image (filename)
  "Dumps clipboard content into an image file FILENAME."
  (let ((dir (file-name-directory filename)))
    (if (not (file-directory-p dir))
        (make-directory dir 'parents))
    ; depending on OS type we do it different
    (when *is-wsl*      
      (let ((oldbuf (current-buffer))
            (tempf (make-temp-file "scratch"))
            (res ""))
        (with-current-buffer "*scratch*"
          (shell-command (concat "powershell.exe " (concat *scripts-dir* "clipboard-to-file.ps1") " " filename)))))
    (when *is-macos*
      (shell-command
       (concat "osascript -e 'get the clipboard as «class PNGf»' | sed 's/«data PNGf//; s/»//' | xxd -r -p  > " filename)))))




;;;; Other Emacs Settings
(use-package emacs
  :init
  ;; short response function instead of long one
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Following commands are disabled by default,
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region  'disabled nil)
  (put 'upcase-region    'disabled nil)
  (put 'narrow-to-page   'disabled nil)
  (put 'erase-buffer     'disabled nil)
  (put 'set-goal-column  'disabled nil)
  (put 'list-timers      'disabled nil)
  ;; (put 'Info-edit 'disabled nil)
  ;; (put 'scroll-left 'disabled nil)
  ;; scroll-lock-mode being enabled randomly is infuriating
  (advice-add 'scroll-lock-mode :override (lambda (&rest args)))
  :custom
  (default-frame-alist '((menu-bar-lines 0)
                         (tool-bar-lines 0)
                         (vertical-scroll-bars)))  
  ;; avoid jerky scrolling 
  (scroll-step 1)
  (scroll-margin 4)
  (inhibit-startup-screen nil)
  (initial-scratch-message nil)
  ;; spaces 
  (indent-tabs-mode nil)
  (tab-width 4))

(use-package simple
  :defer 0.1
  :custom
  (kill-ring-max 30000)
  (column-number-mode t)
  (kill-whole-line t)
  (save-interprogram-paste-before-kill t)  
  :config
  (toggle-truncate-lines 1)
  :bind
  ;; remap ctrl-w/ctrl-h
  (:map ctl-x-map
        ("K" . kill-current-buffer)))

;; Using sneaky strategy to minimize user and GC interference 
(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))


;;; Keymap and Keys Organization 

;; Personal Binding Map on C-z
;; while moving zap to M-z
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

;; we will use M-o for outline
(unbind-key "M-o" 'global-map)

;; Facemenu is super useless outside of center-* functions
;; (define-key global-map (kbd "C-z f") 'facemenu-keymap)
(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode)
  ;; otherwise you can't page through help-map
  :bind (:map help-map
              ("C-h" . which-key-C-h-dispatch)))

;;; Appearance
;;;; Fonts
;;;;; Good typefaces to consider 
;; (set-frame-font "Cousine-11" t)
;; (set-frame-font "Iosevka NF-12:light" t)
;; (set-frame-font "Meslo LG S DZ" t)
;; (set-frame-font "Knack NF" t)
;; (set-frame-font "MesloLGSDZ NF" t)
;; (set-frame-font "MesloLGMDZ NF" t)
;; (set-frame-font "CamingoCode" t)
;; (set-frame-font "InputMonoCondensed Light:light" t)
;; (set-frame-font "InputSerifCondensed Light:light" t)
;; (set-frame-font "InputSansCondensed Light:light" t)
;; (set-frame-font "InputMonoNarrow Light:light" t)
;; (set-frame-font "InputSerifNarrow Light:light" t)
;; (set-frame-font "InputSansNarrow Light:light" t)
;; (set-frame-font "InputSerif" t)
;; (set-frame-font "InputSans" t)
;; (set-frame-font "Fira Code" t)
;; (set-frame-font "Fira Mono for Powerline" t)
;; (set-frame-font "FantasqueSansMono NF-11" t)
;; (set-frame-font "Liberation Mono-11" t)
;; (set-frame-font "Consolas-11" t)
;; (set-frame-font "LiterationMonoPowerline NF" t)
;; (set-frame-font "InconsolataForPowerline NF" t)
;;;;; Font Families
(defconst ffe-font-families '(
			      "Anonymous Pro"
			      "Bitstream"
			      "CamingoCode"
			      "Code New Roman"
			      "Consolas"
			      "Cousine"
			      "DejaVu"
			      "Envy Code R"
			      "Fantasque"
			      "Fira"
			      "Hack"
			      "Inconsolata"
			      "Input"
			      "Iosevka"
			      "Knack"
			      "Liberation Mono"
			      "Literation"
			      "Lucida Console"
			      "M+"
			      "Menlo"
			      "Meslo"
			      "Monaco"
			      "Mono"
			      "Monoid"
			      "NotCourierSans"
			      "Pragmata"
			      "Roboto Mono"
			      "Source Code"
			      "mononoki"
			      ))
;; This is regexp to filter out fonts that are not in the `ffe-font-families'
(defconst --filter-fonts (regexp-opt ffe-font-families) "Regular expression that covers possible usability")

;;;;; Selecting A Font for Frame 
(defun ffe-select-typeface ()
  "Choose typefaces for the frame"
  (interactive)
  (set-frame-font (completing-read "Choose font:"
					(cl-concatenate 'list
							'("Iosevka Light-12"
							  "Iosevka-12"
							  "Hack-11"
							  "Meslo LG S DZ-11"
							  "Cousine-12"
							  "InputMonoCondensed-12"
							  "PragmataPro-12")
							(cl-remove-if-not
							 (lambda (e) (string-match-p --filter-fonts e))
							 (font-family-list))
							 ))
		  t))

(bind-key "f" #'ffe-select-typeface ctl-x-t-map)

;;;; Look and Feel
(use-package whitespace
  :bind (:map ctl-x-t-map
              ("SPC" . whitespace-mode)))

(use-package rainbow-mode
  :ensure t
  :bind (:map ctl-x-t-map
              ("r" . rainbow-mode)))

;;;;; Adding crosshairs and it's dependency
(use-package col-highlight
  :quelpa
  (col-highlight :repo "emacsmirror/col-highlight" :fetcher github))

(use-package hl-line+
  :quelpa
  (hl-line+ :repo "emacsmirror/hl-line-plus" :fetcher github))

(use-package crosshairs
  :bind (:map ctl-x-t-map
              ("+" . crosshairs-mode))
  :quelpa
  (crosshairs :repo "emacsmirror/crosshairs" :fetcher github))

;;;;; menubar
;; there's no point in hiding menubar on macos
(when (not *is-macos*)
  (custom-set-minor-mode 'menu-bar-mode nil))
(custom-set-minor-mode 'tool-bar-mode nil)
(custom-set-minor-mode 'scroll-bar-mode nil)

(when (display-graphic-p)
  (custom-set-minor-mode 'mouse-wheel-mode t)
  (custom-set-minor-mode 'blink-cursor-mode nil))

;;;; Theme
(use-package modus-themes
  :ensure t
  :commands (modus-themes-toggle)
  :init
  (setf modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax '(alt-syntax green-strings yellow-comments)
        modus-themes-org-blocks 'gray-background
        modus-themes-hl-line '(accented)
        modus-themes-headings '((1 . (rainbow background)) (t . (rainbow background overline))))
  (load-theme 'modus-operandi)
  :config
  (if (display-graphic-p)
      (load-theme 'modus-operandi)
    (load-theme 'modus-vivendi))
  :bind
  (:map ctl-x-t-map
        ("t" . modus-themes-toggle)))


;; (use-package apropospriate-theme
;;   :ensure t
;;   :config 
;;   ;(load-theme 'apropospriate-dark t)
;;   ;; or
;;   (load-theme 'apropospriate-light t)
;;   )

;;;; Wrapping and Visual Lines
(visual-line-mode 1)
;; These are good to use with org-mode, so it doesn't change paragraph by inserting newlines. 
(use-package adaptive-wrap
  :ensure t
  :hook (visual-fill-column-mode . adaptive-wrap-prefix-mode))
(use-package visual-fill-column
  :ensure t
  :hook visual-line-mode)
(use-package visual-fill
  :ensure t)

;;; Files
;;;; Project Files
(use-package find-file-in-project
  :ensure t
  :config (setq ffip-prefer-ido-mode nil
                ffip-match-path-instead-of-filename t)
  :bind (:map ctl-x-f-map
              ("f" . find-file-in-project)
              ("s" . find-file-in-current-directory)))

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

;;;; Recent Files
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
	  recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)))

  :config
  (progn
    (add-to-list 'recentf-exclude
	         (expand-file-name *data-dir*))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

  (defun find-select-recent-file ()
    "Use `completing-read' to \\[find-file] a recent file"
    (interactive)
    (unless recentf-mode
      (recentf-mode t))
    (if (find-file (completing-read "Find recent file: " recentf-list))
       (message "Opening file...")
      (message "Aborting")))

  :bind (:map ctl-x-f-map
              ("r" . counsel-recentf)
              ("R" . recentf-open-most-recent-file)))

;;;; Generic Finding Files
(use-package ffap
  :bind (:map ctl-x-f-map
              ("RET" . find-file-at-point)))

;;;; Dired
(use-package dired
  :init
  (progn  (add-hook 'dired-mode-hook #'hl-line-mode)
          (put 'dired-find-alternate-file 'disabled nil)
          (when (string= system-type "darwin")       
            (setq dired-use-ls-dired nil)))
  :config
  (unbind-key "M-s f" dired-mode-map)
  (unbind-key "M-s a" dired-mode-map)

  (defun ffe-dired-do-delete (&optional arg)
    "Just like `dired-do-delete' but without too many confirmations"
    (interactive "P")
    (let ((dired-recursive-deletes 'always)
          (dired-deletion-confirmer (lambda (arg) t)))
      (dired-do-delete arg)))

  (bind-key [remap dired-do-delete] #'ffe-dired-do-delete dired-mode-map))

(bind-key "d" #'counsel-dired ctl-x-map)
(bind-key "j" #'counsel-dired-jump ctl-x-f-map)


;;; Navigation and Visibility 
;; navigation in a buffer  and visibility of the buffer content 
;; TODO: add narrow/widen
;;;; Bookmarks
(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat *data-dir* "bookmarks")
	bookmark-save-flag 1))

;;;; Outline and Imenu
(use-package imenu
  :commands (imenu)
  :bind (:map search-map
              ("i" . imenu)))

;; (use-package imenu+
;;   :after (imenu))

(use-package imenu-list
  :after (imenu)
  :commands (imenu-list)
  :ensure t
  :bind (:map search-map
              ("I" . imenu-list)))

(use-package imenu-anywhere
  :after (imenu)
  :commands (imenu-anywhere)
  :ensure t
  :bind (:map search-map
              ("M-i" . imenu-anywhere)))

(use-package outshine
  :ensure t
  :diminish "OutS"
  :init (defvar outline-minor-mode-prefix (kbd "M-o"))
  :config (add-hook 'outline-minor-mode-hook 'outshine-mode))

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


;;;; Visibility

;; Visibility of the text in a window
;; C-z /          hides lines matching regexp
;; C-u C-z /      hides lines not matching regexp
;; C-u C-u C-z /  unhides everything
(use-package hide-lines
  :ensure t
  :defer t
  :bind (:map ctl-z-map
	      ("/" . hide-lines)))

;;;; OnScreen
;; moving onto anything that is visible on the screen
(use-package ace-jump-mode
  :ensure t
  :bind (:map goto-map
	 ("j" . ace-jump-mode)))

;;;; Line numbers
(use-package display-line-numbers
  :bind (:map ctl-x-t-map
              ("l" . display-line-numbers-mode)))

;;; Search

;; Searching in files and in buffers
;;
;; Consistent keys in Grep and Occur modes unifying `moccur-mode-map', `grep-mode-map', `occur-mode-map'
;; 
;; p - previous select
;; n - next select
;; M-p - prev without  selection
;; M-n - next  without selection
;; M-{, { - prev file (buffer)
;; M-}, } - next file (buffer)
;; TODO: make moccur windows work like occur

;;;; Grep
;; Grep is available on any system but it's the most simple
(use-package grep
  :commands (grep rgrep grep-find)
  :bind (("M-s g" . grep)
	 ("M-s f" . grep-find)
	 ("M-s r" . rgrep)))
;; writable grep - propagate changes to the files from the grep buffer
;; Start with C-c C-p edit grep buffer
;; C-c C-c finish editing
;; C-c C-k abort changes
;; wgrep-save-all-buffers saves all modified buffers
(use-package wgrep :ensure t :defer t)

;;;; Occur

(use-package color-moccur
  :ensure t
  :commands isearch-moccur-all
  :bind (("M-s M-o" . moccur)
	 :map isearch-mode-map
	 ("M-s M-o" . isearch-moccur-all)
	 :map moccur-mode-map
	 ("M-p" . moccur-prev)
	 ("M-n" . moccur-next)
	 ("M-{" . moccur-prev-file)
	 ("M-}" . moccur-next-file)
	 ("{" . moccur-prev-file)
	 ("}" . moccur-next-file)))

(use-package replace
  :config
  (progn
    (defun occur-symbol-at-point ()
      "Calls `occur' with a symbol at point"
      (interactive)
      (let ((s (symbol-name (symbol-at-point))))
        (push s regexp-history)
        (occur s)))

    (defun occur-prev-buffer ()
      "moves to the occur entry in the previous buffer"
      (interactive)
      (if (re-search-backward "^[0-9]+ matches? in buffer: " nil t 2)
          (occur-next 1)
        (goto-char (point-max))
        (if (re-search-backward "^[0-9]+ matches? in buffer: " nil t)
	    (occur-next 1))))

    (defun occur-next-buffer ()
      "Moves to the occur entry in the next buffer"
      (interactive)
      (if (re-search-forward "^[0-9]+ matches? in buffer: " nil t)
          (occur-next 1)
        (goto-char (point-min))
        (occur-next 1)))

    (defun occur-prev-error ()
      "Goes to the previous occur entry"
      (interactive)
      (occur-next-error -1))

    (defun occur-prev-noselect ()
      "Pops up buffer location for the occur but stays in *Occur* buffer"
      (interactive)
      (occur-prev-error)
      (other-window 1))

    (defun occur-next-noselect ()
      "Pops up buffer location for the occur but stays in *Occur* buffer"
      (interactive)
      (occur-next-error 1)
      (other-window 1)))
  
  :bind (("M-s O" . multi-occur)
	 ("M-s m" . multi-occur-in-matching-buffers)
	 ("M-s M-s" . occur-symbol-at-point)
	 :map occur-mode-map
	 ("M-n" . occur-next-error)
	 ("M-p" . occur-prev-error)
	 ("{" . occur-prev-buffer)
	 ("M-{" . occur-prev-buffer)
	 ("}" . occur-next-buffer)
	 ("M-}" . occur-next-buffer)
	 ("n" . occur-next-noselect)
	 ("p" . occur-prev-noselect)))


;;;; Ag
;; requires `ag' tool to be installed on the system
;; This is a custom version of the library that should be loaded from
;; the git submodule
(use-package ag
  :ensure t
  :defer t
  :ensure-system-package (ag . silversearcher-ag)
  :custom
  (ag-highlight-search t "Highlight the current search term.")
  (ag-reuse-buffers t)
  (ag-reuse-window t))

(use-package wgrep-ag :ensure t :defer t)

;;;; Isearch
;; These are handy keys when you are navigating a buffer using isearch
(use-package isearch
  :config (setq isearch-allow-scroll t)
  :bind (:map isearch-mode-map
              ("<up>" . isearch-repeat-backward)
              ("<down>" . isearch-repeat-forward)))

;;; Editing
;;;; Multiple Modes
(use-package polymode
  :ensure t)
;;;; Editing Operations
(use-package misc
  :commands (zap-up-to-char forward-to-word)
  ;; zapping back is done via negative argument C-- or M--
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)
         ("M-f" . forward-to-word)
         ("M-F" . forward-word)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode  
  :config (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-toggle-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,*undo-dir*))))

(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :init 
  (use-package browse-kill-ring+
    :defer 10)
  :bind (("C-M-y" . browse-kill-ring)))

;; TODO activate it later
(use-package visual-regexp
  :ensure t
  :bind (:map ctl-z-map
         ("%" . vr/query-replace)))

;;;; Unfill paragraph or region
(use-package unfill
  :ensure t)

;;;; Spellcheck
;; On Windows use pre-compiled hunspell
;; For Mac OS X install `aspell' using Homebrew
;;
;;     brew install aspell --with-all-langs
;; 
;; In order for spelling to work correctly it expects DICTIONARY
;; environment variable to be set
;; Trick from http://www.emacswiki.org/emacs/InteractiveSpell
(setenv "DICTIONARY" "en_US")

;; note that for aspell file has to have a first line
;; personal_ws-1.1 en 0
;;    https://blog.samat.org/2008/11/02/creating-your-own-personal-aspell-dictionary/
(defun ffe-personal-dictionary ()
  "Personal dictionary setup if file doesn't exist then create it"
  (let ((personal-dictionary-file (concat *data-dir* ".personal.dict")))
    (unless (file-exists-p personal-dictionary-file)
      (with-temp-file personal-dictionary-file t))
    personal-dictionary-file))

(use-package flyspell
  :commands (flyspell-buffer flyspell-mode flyspell-region)
  :config (setq flyspell-use-meta-tab nil
		flyspell-auto-correct-binding (kbd "C-M-;"))
  :bind (:map flyspell-mode-map
	      ("C-," . nil)
	      ("C-." . nil)
	      ("C-;" . nil)
	      ("C-c $" . nil)
	      ("C-$ n" . flyspell-goto-next-error)
	      ("C-$ b" . flyspell-buffer)
	      ("C-$ r" . flyspell-region)))

(use-package ispell
  :defer t
  :config (setq ispell-personal-dictionary (ffe-personal-dictionary)
		ispell-silently-savep t
		ispell-choices-win-default-height 3)
  :init (progn
	  ;; Aspell Specific
	  (when (executable-find "aspell")
	    (setq ispell-program-name "aspell"
		  ispell-extra-args '("--sug-mode=ultra")))
	  ;; Hunspell Specific
	  (when (executable-find "hunspell")
	    (setq ispell-program-name "hunspell"))))

;;;; Using LangTool
;; Obtain langtool (https://languagetool.org/)
;; In the terminal run
;;     mkdir -p $HOME/.local/share
;;     curl -o langtool.zip https://languagetool.org/download/LanguageTool-stable.zip
;;     unzip langtool.zip -d $HOME/.local/share/
;;     rm -f langtool.zip 
;;  See https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
  :ensure t
  :init (setf langtool-language-tool-jar "~/.local/share/LanguageTool-5.5/languagetool-commandline.jar"))


;;; Windows and Buffers
;;;; Windows
(use-package windmove
  :ensure t
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
  :bind (:map ctl-x-w-map
              ("w" . ace-window)))

(defun toggle-window-split ()
  "Changes frame split from horizontally divided windows to vertically divided windows and vice versa"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(bind-key "|" 'toggle-window-split ctl-x-w-map)

;;;; Buffer operations
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


(defun ffe-swap-buffers-with-window ()
  "Moves current buffer to the given WINDOW."
  (interactive)
  (let ((this-buffer (current-buffer))
        (this-window (selected-window)))
    ; this will use ace to select window and perform action
    (aw-select "Ace - Move Buffer"
               (lambda (window)
                 (let ((that-buffer (window-buffer window)))
                   (message  "Window: %s  Buffer: %s" window this-buffer)
                   (set-window-buffer this-window that-buffer)
                   (set-window-buffer window this-buffer)
                   (select-window window))))))

(bind-key "w" 'ffe-swap-buffers-with-window ctl-x-x-map)

;;; Help
;; Help/Info configuration
;;;; Emacs Help
;(use-package help-mode+)
;(use-package help+)
;(use-package help-fns+)

;; How often I need to see emacs FAQ. Use C-h C-f as a keymap for find-* commands
(defvar help-find-map)
(define-prefix-command 'help-find-map)
(bind-key "C-f" 'help-find-map help-map)

;; I don't really need to see view-hello-file either 
(defvar help-help-map)
(define-prefix-command 'help-help-map)
(bind-key "h" 'help-help-map help-map)


(use-package help
  :config (progn
            (defun ffe-describe-symbol-at-point ()
              "Describes symbol at a point"
              (interactive)
              (when-let (symbol (symbol-at-point))
                (describe-symbol symbol)))

            (defun ffe-quit-windows-on-help ()
              "Quits window with *Help* buffer"
              (interactive)
              (quit-windows-on "*Help*")))  
  :bind (:map help-map
              ("C-b" . describe-personal-keybindings)
              ("C-k" . describe-key-briefly)
              ("C-c" . describe-char)
              ("C-s" . ffe-describe-symbol-at-point)
              ("C-S-s" . describe-symbol)
              ;; Too often I need to close Help windows after seeing the doc and I don't really use `help-quit' anyway
              ([remap help-quit] . ffe-quit-windows-on-help)
              ;; help-for-help is nice, but I would like C-h still show me prefix bindings
              ("C-h" . describe-prefix-bindings)
              
         ;; finding a variable or a function or a library, i.e. opening them in the editor
         :map help-find-map
              ("f" . find-function)
              ("v" . find-variable)
              ("l" . find-library)))

(use-package helpful
  :ensure t
  :commands (helpful-at-point
             helpful-callable
             helpful-command
             helpful-function
             helpful-key
             helpful-macro
             helpful-variable)  
  :bind
  ([remap display-local-help] . helpful-at-point)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))


;;;; Info

;;; Calendar
(use-package calendar
  :config
  (setq diary-file (concat *data-dir* "diary")))

;;; Completion
;;;; In a Buffer
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

;;;; In the Minibuffer
;; Let's use Ivy and Counsel
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode)
  :bind (("M-o j" . counsel-outline)
         ("M-g f" . counsel-flycheck)
         ([remap insert-char] . counsel-unicode-char)
         ("M-s s" . counsel-grep-or-swiper)
         ("M-s a" . counsel-ag)
         ("M-g m" . counsel-mark-ring)))

(use-package ivy
  :ensure t
  :demand t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :after ivy)


;;; Expandable Snippets
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

;;; Version Control
(use-package magit
  :ensure t
  :commands magit-status
  :bind (:map ctl-z-map
	      ("g" . magit-status)))

(use-package monky
  :ensure t
  :commands monky-status
  :bind (:map ctl-z-map
	      ("h" . monky-status)))

;;; Programming Modes
;;;; General Settings 
(use-package eldoc
  :diminish eldoc-mode
  :commands global-eldoc-mode
  :config
  (global-eldoc-mode))

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

;;;; Case: Camel, Pascal, Snake, etc.
(use-package string-inflection
  :ensure t
  :commands string-inflection-all-cycle
  :defer t
  :bind (:map ctl-x-t-map
              ("." . string-inflection-all-cycle)))


;;;; Language Server
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-z .")
  :hook
  ((js2-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom 
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable nil)
  :after lsp-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :after lsp-mode)

(use-package dap-mode
  :ensure t
  :commands (dap-debug dap-debug-edit-template dap-register-debug-template)
  :after lsp-mode
  :config
  (require 'dap-python)  
  :bind
  (:map lsp-mode-map        
        ("C-z . d" . dap-debug))
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode)))

;;;; Lisp
;;;;; General Lisp 
(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook
  (emacs-lisp-mode                  . enable-paredit-mode)
  (ielm-mode                        . enable-paredit-mode)
  (lisp-mode                        . enable-paredit-mode)
  (lisp-interaction-mode            . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ;; we use M-s for searching stuff
        ("M-s" . nil)
        ;; bind splice onto M-k
        ("M-k" . paredit-splice-sexp)))

;; (use-package parinfer-rust-mode
;;   :ensure t
;;   :custom
;;   (parinfer-rust-disable-troublesome-modes 't)
;;   (parinfer-rust-check-before-enable 'defer)
;;   (parinfer-rust-auto-download nil))

;;;;; Emacs Lisp
(use-package emacs-lisp-mode
  :config
  (defun ffe-disable-elisp-checkdoc-in-configuration-files ()
    "turn off checkdoc for my configuration files"
    (if (and (eq major-mode 'emacs-lisp-mode) ; if it is elisp
		     (or
		      (string-equal user-init-file (buffer-file-name)) ; or init.el file
		      (string-equal custom-file (buffer-file-name)) ; customization file
		      (and                      ; configuration modules
		       (string-match (or (file-name-directory (or  (buffer-file-name) "")) "") *lisp-dir*)
		       (string-match "^ffe-" (or (file-name-nondirectory (or  (buffer-file-name) "")) "")))))
        (flycheck-disable-checker 'emacs-lisp-checkdoc)))
  (add-hook 'emacs-lisp-mode-hook #'ffe-disable-elisp-checkdoc-in-configuration-files)
  ;; (global-set-key [remap eval-expression] 'pp-eval-expression)
  ;; (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-buffer)))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode ielm-mode lisp-mode lisp-interaction-mode eval-expression-minibuffer-setup) . elisp-slime-nav-mode))

(use-package ielm
  :defer t
  :init
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
  :bind
  ("C-M-:" .  ffe-ielm)
  (:map paredit-mode-map
        ("RET" . nil)))

;;;; Clojure
(use-package cider
  :after (paredit eldoc)
  :ensure t
  :commands (cider-jack-in)
  :config
  (dolist (hook '(cider-mode-hook cider-repl-mode-hook clojure-mode-hook clojurescript-mode-hook))
    (add-hook hook #'paredit-mode)
    (add-hook hook #'eldoc-mode)))

(use-package 4clojure
  :commands (4clojure-open-question)
  :after cider
  :ensure t
  :defer t
  :init (progn
	  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
	    "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
	    ad-do-it
	    (unless cider-current-clojure-buffer
	      (cider-jack-in)))))


;;;; C/C++
;; C/C++ Configuration mostly for one off c/c++ programs not for complex C/C++ projects
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode)))

;; C/C++ Headers Locations. This is system specific 
(defconst *ffe-c-headers-dirs* '("C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++\\x86_64-w64-mingw32"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++\\backward"
				 "C:\\PF\\LLVM\\lib\\clang\\3.7.0\\include"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include-fixed"
				 "C:\\mingw64\\x86_64-w64-mingw32\\include"
				 "C:\\mingw64\\include"))

(use-package company-c-headers
  :defer t
  :commands company-c-headers
  :ensure t
  :config
  (dolist (dir (if (boundp '*ffe-c-headers-dirs*)
		   *ffe-c-headers-dirs*
		 ()))
    (add-to-list 'company-c-headers-path-system dir)))

(use-package c-eldoc
  :defer t
  :commands c-turn-on-eldoc-mode
  :ensure t
  :config (setq c-eldoc-includes
                (mapconcat #'identity
                           ;; on Windows `pkg-config` .... leads to an
                           ;; error
                           (cons ;c-eldoc-includes
                                 "-I. -I.."
                                 (mapcar (apply-partially #'concat "-I")
                                         *ffe-c-headers-dirs*))
                           " ")
                c-eldoc-cpp-command "cpp"))

(defun ffe-c-mode-hook ()
  "This is settings for the C/C++ mode

Due to a bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759 add it to a c-mode-common-hook"
  (when (memq major-mode '(c-mode c++-mode))
    (electric-pair-mode +1)
    (electric-indent-local-mode +1)
    (c-toggle-hungry-state +1)
    (c-set-style "gnu")
    (setq c-basic-offset 4)
    (c-turn-on-eldoc-mode)
    (set (make-local-variable 'compile-command)
         (let ((f (file-name-nondirectory (or (buffer-file-name) ""))))
           (case major-mode
             ('c-mode (format "gcc -g -O2 -std=gnu99 -static -lm %s" f))
             ('c++-mode (format "g++ -g -O2 -static -std=gnu++11 %s" f))
             (t compile-command))))

    (ffe-add-company-backends 'company-c-headers 'company-semantic 'company-clang 'company-xcode)))

(add-hook 'c-mode-common-hook #'ffe-c-mode-hook)

;;;; Go
;; Go language setup
;;
;; Requires GOPATH and PATH to be setup and include go executables
;;
;; Documentation:
;; Oracle    https://docs.google.com/document/d/1SLk36YRjjMgKqe490mSRzOPYEDe0Y_WQNRv-EiFYUyw/view
;; Guru    https://docs.google.com/document/d/1_Y9xCEMj5S-7rv2ooHpZNH15JgRT5iM742gJkw5LtmQ/edit

;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/lint/golint
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode

;; TODO:
;; package-install flymake-go
;; go get -u github.com/dougm/goflymake
;; other packages to look at
;;  go-add-tags
;;  go-fill-struct
;;  go-gen-test
;;  go-impl
;;  go-projectile
;;  go-rename
;;  go-tag
;;  golint

(use-package go-mode
  :ensure t
  :init (progn
          (use-package go-eldoc
            :ensure t
            :init (add-hook 'go-mode-hook #'go-eldoc-setup))
          (use-package go-guru
            :ensure t
            :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))
          (use-package company-go
            :ensure t
            :init (add-hook 'go-mode-hook (lambda () (ffe-add-company-backends 'company-go))))

          (defun go-run-buffer ()
            "This will run buffer on the Go"
            (interactive)
            (compile (concat "go run " (buffer-file-name))))

          (add-hook 'go-mode-hook (lambda ()
                                    ;; customize  compile command for go-mode
                                    (set (make-local-variable 'compile-command)
                                         "go build")
                                    ;; make before-save-hook local for go-mode buffer
                                    (add-hook 'before-save-hook 'gofmt nil t)))
          (bind-keys :map go-mode-map
                     :prefix "C-c C-d"
                     :prefix-map go-mode-doc-map
                     ("h"   . godoc)
                     ("d"   . godef-describe)
                     ("C-d" . godoc-at-point)))
  
  :config (progn
            (setq gofmt-command "goimports"))
  
  :bind (:map go-mode-map
         ("C-c C-c" . go-run-buffer)))


;;;; Javascript
;;
;; We use lsp-server with NodeJS
;; see https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;; could be installed via `lsp-install-server' then choose 'ts-ls
;; 
(use-package js2-mode
  :defer t
  :diminish (javascript-mode . "JS")
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (progn
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook #'imenu-add-menubar-index)
    (add-hook 'js2-mode-hook #'idle-highlight-mode)
    (add-hook 'js2-mode-hook #'electric-pair-mode)
    (add-hook 'js2-mode-hook #'hs-minor-mode)))

;;;;; Customize Projectile
(when (fboundp 'projectile-register-project-type)
  (projectile-register-project-type 'npm '("package.json") :test "npm test"  :test-suffix ".spec"))

;;;; Python 
;;
;;Python configuration. Instead of pylsp we use pyright
;; 
(use-package python
  :defer t
  :commands python-mode
  :config
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'python-mode-hook #'lsp-deferred)
  :bind
  (:map python-mode-map
	;; python-eldoc-at-point is not really useful, instead
	;; use it for sending file to python shell
	("C-c C-f" . python-shell-send-file)
	("C-M-f" . python-nav-forward-sexp)
	;; moves between syntactic blocks (if,for,while,def,..)
	("C-M-b" . python-nav-backward-sexp)
	("M-}" . python-nav-forward-block)
	("M-{" . python-nav-backward-block)
	;; In python this looks like next line skipping
        ;; comments and multi-line strings
	("M-e" . python-nav-forward-statement)
	("M-a" . python-nav-backward-statement)))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  ;; :hook (python-mode . (lambda ()
  ;;                        (require 'lsp-pyright)
  ;;                        (lsp-deferred)))
  )

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)))

;;;; Rust

;; Configuration for the Rust programming language
;; 
;; See also: https://hoverbear.org/2017/03/03/setting-up-a-rust-devenv/
;; 
;; This setup expects that one has installed rust system using
;; `rustup-init'. Need to set default toolchain. To see which one is
;; default use:
;; 
;;     rustup show
;; Install `rustfmt'
;;     
;;   and `racer' globally 
;; There's a `cargo' tool and we installed `racer' and
;; `rustfmt' crates
;; 
;; 
;; Check out corresponding documentation on how to
;; do that. More over there's a RUST_SRC_PATH environment variable
;; pointing to rust sources.
(use-package rust-mode
  :ensure t
  :defer t)

(use-package cargo
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; Auto completion for rust
(use-package racer
  :ensure t
  :defer t
  :after rust-mode
  :init (add-hook 'rust-mode-hook #'racer-mode)   
  :config (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

;; Major mode for .toml Cargo files, I don't think it's used anywhere
;; outside of Rust ecosystem so it stays here for now
(use-package toml-mode
  :ensure t
  :defer t)

;;;; Groovy
(use-package groovy-mode
  :ensure t)

;;;; C#
(use-package csharp-mode
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'lsp-deferred))


;;;; F#

;; Download netcore release from https://github.com/fsharp/FsAutoComplete
;; and unzip  it in $HOME/.FsAutoComplete/netcore
;;
;; another option could be using https://github.com/fsprojects/fsharp-language-server
;; (see Readme about how to build)
;; Once you have it built add ./src/FSharpLanguageServer/bin/Release/net6.0/linux-x64/ or whatever platform is to path
;;
(defun ffe-fsharp-ls-setup ()
  "Initializes FSharpLanguageServer as a backend for LSP"
  (require 'lsp)
  ;; this one seems more robust 
  (when-let ((fsharp2-lsp-executable (executable-find "FSharpLanguageServer")))
      (progn
        (setq lsp-fsharp-server-path "")        
        ;; creating client for fsharp-ls
        (lsp-register-client
         (make-lsp-client
          :new-connection (lsp-stdio-connection fsharp2-lsp-executable)
          :major-modes '(fsharp-mode)
          :server-id 'fsharp-lsp
          :notification-handlers (ht ("fsharp/startProgress" #'ignore)
                                     ("fsharp/incrementProgress" #'ignore)
                                     ("fsharp/endProgress" #'ignore))
          :priority 1))))
  (lsp))

(use-package fsharp-mode
  :ensure t
  :config
  (add-hook 'fsharp-mode-hook #'ffe-fsharp-ls-setup)
  :init
  (setf lsp-fsharp-server-install-dir "~/.FsAutoComplete/netcore/"
        lsp-fsharp-external-autocomplete t))

;; (use-package eglot-fsharp
;;   :ensure t
;;   :after fsharp-mode eglot
;;   :init
;;   (setf eglot-fsharp-server-install-dir "~/.FsAutoComplete/"))


;;;; Ocaml
;; Emacs’ OCaml mode
(use-package tuareg
  :ensure t
  :config
  (setq tuareg-support-metaocaml t           ;; Only handles metaocaml syntax
        org-babel-ocaml-command "metaocaml"))  ;; Different command for metaocaml

; (async-shell-command "time opam install ocp-indent merlin") ;; real 1m33.636s
(use-package merlin
  :ensure t
  :init
  (setf merlin-command 'opam)
  :config
  (autoload 'merlin-mode "merlin" nil t nil)
  :hook
  (tuareg-mode . merlin-mode)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("C-c i" . merlin-locate-indent)
              ("C-c C-o" . merlin-occurences)
              ("C-c C-j" . merlin-jump)))

(use-package utop
  :ensure t
  :config
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :init
  (setf utop-command "opam config exec -- utop -emacs")
  :hook
  (tuareg-mode . utop-minor-mode))

;;;; Lua
(use-package lua-mode
  :ensure t)

;;;; Powershell
(use-package powershell
  :ensure t)

;;;; Shaders
(use-package company-glsl
  :ensure t
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

;;; TeX Mode
;;;; TeX Settings
(use-package tex-site                   ; AucTeX initialization
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-parse-self t)      ; parse documents for auto completion
  (TeX-auto-save  t)      ; parse on save
  (TeX-auto-untabify t)   ;
  (TeX-display-help t)    ;
  (TeX-save-query nil)    ;
  (TeX-clean-confirm nil) ;
  (TeX-master nil)        ;
  (TeX-check-TeX nil)     ; we may only have ConTeXt on Windows
  (TeX-PDF-mode t)
  (TeX-file-extensions
   '("mkii" "mkiv" "tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx"))
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
  (TeX-view-program-list
   '(("PDF Tools" TeX-pdf-tools-sync-view))) 
  :config
  (add-hook 'TeX-mode-hook #'TeX-source-specials-mode)
  (add-hook 'TeX-mode-hook #'TeX-toggle-debug-bad-boxes)
  (add-hook 'TeX-mode-hook #'TeX-toggle-debug-warnings)
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  (add-hook 'TeX-mode-hook #'abbrev-mode)
  (add-hook 'TeX-mode-hook #'auto-fill-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package pdf-tools
  :ensure t
  :init
  (pdf-loader-install))

;;;; ConTexT Specifics
(use-package context
  :defer t
  :mode (("\\.mkiv\\'" . context-mode)
         ("\\.mkii\\'" . context-mode))
  :custom
  (ConTeXt-Mark-version "IV")
  :config
  (setq revert-without-query '(".+pdf$"))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'ConTeXt-mode-hook
            (lambda ()
              ;; Install digestif
              ;; https://github.com/astoff/digestif
              ;;
              ;; if we are using digestiff with eglot we need to send correct language id to
              ;; LSP server
              (setq lsp-tex-server 'digestiff)
              (put 'ConTeXt-mode 'eglot-language-id "context")
              (eglot))))

;; let's define polymode for metapost in ConTeXt
(define-hostmode poly-context-hostmode :mode 'ConTeXt-mode)
(define-innermode poly-context-metapost-innermode
  :mode 'metapost-mode
  :head-matcher "\\\\startMPcode"
  :tail-matcher "\\\\stopMPcode"
  :head-mode 'host
  :tail-mode 'host
  :init-functions)
(define-polymode poly-context-mode
  :hostmode 'poly-context-hostmode
  :innermodes '(poly-context-metapost-innermode))
;(add-hook 'ConTeXt-mode-hook 'poly-context-mode)

;;;; Metapost
(use-package meta-mode
  :defer t)
;;;;; Preview Metapost Buffer via MPtoPDF
(use-package metapost-mode+
  :defer t)



;;; Misc File Formats
;; Various file formats
;;;; JSON
(use-package json-mode
  :defer t
  :ensure t
  :init (progn
          (use-package json-navigator
            :ensure t)
          (use-package json-snatcher
            :commands (jsons-print-path)
            :ensure t)
          (add-hook 'json-mode-hook #'hs-minor-mode))
  :bind (:map json-mode-map
              ("C-c p" . jsons-print-path)))

;;;; NGinx Configuration
(use-package nginx-mode
  :ensure t
  :commands nginx-mode)

;;;; YAML
(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode))
  :hook (yaml-mode . yaml-mode-outline-hook)
  :init
  (defun yaml-outline-level ()
    "Returns level based on indentation"
    (s-count-matches (concat "[ ]\\{" (number-to-string  yaml-indent-offset) "\\}") (thing-at-point 'line t)))

  (defun yaml-mode-outline-hook ()
    (outline-minor-mode)
    ; "^\\(\\s-\\{2\\}\\)*\\(['][^']*[']\\|[\"][^\"]*[\"]\\|[a-zA-Z0-9-_/:.}{]+\\):\\s-*$"
    ;; (setf outline-regexp "^\\(\\s-\\{2\\}\\)*\\(['][^']*[']\\|[\"][^\"]*[\"]\\|[a-zA-Z0-9-_/:.}{]+\\):\\s-*$")
    ;; (setf outline-level 'yaml-outline-level)
    ))
;;;; Markdown
(use-package markdown-mode  
  :ensure t
  :mode (("\\.md$" . markdown-mode))
  :config (add-hook 'markdown-mode-hook #'flyspell-mode)

  (setq markdown-command "pandoc")

  (defun flyspell-markdown-check-word-predicate ()
    "Used by `flyspell-mode' in Markdown documents to skip spell checking code blocks and inline code"
    (let ((face (get-text-property (-  (point) 1) 'face)))
      (not (memq face '(markdown-pre-face markdown-inline-code-face markdown-reference-face)))))

  
  (defun markdown-plus-enter-key ()
    "Modification of enter key that just jumps onto the next line if ENTER key is pressed while point is on header"
    (interactive)
    (if (thing-at-point-looking-at markdown-regex-header)
	(let ((next-line-add-newlines t))
          (next-line))
      (markdown-enter-key)))

  (defun markdown-plus-current-header-level ()
    "Return level of the header for the current position"
    (cond ((markdown-heading-at-point) (markdown-outline-level))
          ;; check previous heading 
          (t (save-excursion
               (goto-char (markdown-previous-heading))
               (markdown-outline-level)))))

  (defun markdown-plus-insert-new-header-same-level ()
    "Creates a new header of the same level and moves point onto it"
    (interactive)
    (let ((current-level (markdown-plus-current-header-level)))
      (markdown-insert-header current-level "X")
      ;; now delete that extra char, i.e. X
      (delete-char -1)))

  (bind-keys :map markdown-mode-map
             ("RET" . markdown-plus-enter-key)
             ;; C-RET inserts new heading current level
             ("<C-return>" . markdown-plus-insert-new-header-same-level)
             ;; M-<left> promotes, M-<right> demotes
             ("<M-left>"  . markdown-promote)
             ("<M-right>" . markdown-demote))
  ;; TODO:  M-<up> - folds current entry, M-<down> - unfolds current entry
  
  
  :init (setq flyspell-generic-check-word-predicate
	      'flyspell-markdown-check-word-predicate))

;;;; LESS 
(use-package less-css-mode
  :ensure t)

;;;; CSV
(use-package csv-mode
  :ensure t)

;;;; Shell Scripts 
(use-package sh-script
  :mode (("\\.zsh" . sh-mode)))
;;;; RESTClient
(use-package restclient
  :ensure t)

;;;; PlantUML
;; On Mac OS X plantuml could be installed via brew
;; 
;; For Org-mode this requires plantuml.jar to be available
;; and specified via `org-plantuml-jar-path'
(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-exec-mode 'executable))

;;;; Justfile
(use-package just-mode
  :ensure t)
;;; Org Mode
;; comment out ob-ipython as it gives error if no ipython is installed
;; (use-package ob-ipython
;;   :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '(
                                        ;(ipython . t)
               (ruby . t)
               (python . t)
               (emacs-lisp . t)
               (latex . t)
               (gnuplot . t)
               (C . t)
               (shell .t)
               (awk . t)
               (plantuml . t)))

            ;; Refiling - allow creating new targets
            (setq org-refile-allow-creating-parent-nodes 'confirm
                  org-refile-targets '((org-agenda-files :maxlevel . 5)))

;;;; Useful Commands used in Org-Mode
;;;;; Jump start/end in code block            
            (defun ffy-org-goto-block-begin/end (p)
              "Go to begining/end of the current block. With prefix goes to the end.

This one will jump between BEGIN and END of org mode blocks
https://github.com/kshenoy/dotfiles/blob/master/emacs.org#jump-to-headtail-of-any-block-not-just-src-blocks
"
              (interactive "P")
              (let* ((element (org-element-at-point)))
                (when (or (eq (org-element-type element) 'example-block)
                          (eq (org-element-type element) 'src-block))
                  (let ((begin (org-element-property :begin element))
                        (end (org-element-property :end element)))
                    ;; ensure point is not on a blank line after the block
                    (beginning-of-line)
                    (skip-chars-forward " \r\t\n" end)
                    (when (< (point) end)
                      (goto-char (if p end begin))
                      (when p
                        (skip-chars-backward " \r\t\n")
                        (beginning-of-line)))))))

;;;;; Insert Screenshot into org-mode file
            (defun ffe-org-insert-screenshot (&optional arg)
              "Runs a program and takes screenshot, then writes it into a file and then inserts link to org buffer.

Files are named after the Org headline, by replacing non-character with dashes.

If ARG is nil then images are droppedinto a directory (created if it doesn't exist) that is named after org file.
If ARG is 4, i.e. C-u is pressed, then puts image into a directory (created if it doesn't exist) /img.
If ARG is 16, i.e. C-u C-u is pressed, just drop image file alongside the org file.
"
              (interactive "p")              
              (let* ((org-file-name (buffer-file-name))                     
                     (org-header (car (cddddr (org-heading-components))))
                     (filename-header-part (replace-regexp-in-string "\\W+" "-" org-header nil 'literal))
                     ;; Figure out directory to put images to
                     (image-directory
                      (cond
                       ;; C-u
                       ((eq arg 4) (ffe-image-directory org-file-name 'img))
                       ;; C-u C-u
                       ((eq arg 16) (ffe-image-directory org-file-name))
                       (t (ffe-image-directory org-file-name 'file))))
                     ;; full path to file name
                     (image-file-name (ffe-image-filename image-directory filename-header-part))
                     ;; file name path relative to org-file-name
                     (relative-image-file-name
                      (replace-regexp-in-string
                       (file-name-directory org-file-name) "" image-file-name nil 'literal)))
                ;; Ensure that directory is created
                (if (not (file-directory-p image-directory))
                    (make-directory image-directory 'parents))                
                
                ;; Turns out we can have issues writing into files
                ;; that are in directory pointed to by symbolic link
                ;; We will write through temporary file
                (let ((temp-file (make-temp-file "clipimg")))                  
                  (with-temp-file temp-file
                    (ffe-clipboard-to-image temp-file)
                    ()
                    (copy-file temp-file image-file-name t)
                    (delete-file temp-file)))
                (insert (concat "[[file:" relative-image-file-name "]]"))
                (org-display-inline-images)))
            )
;;;;; Remove Bookmark Faces
  (defun ffe-reset-bookmark-faces ()
    "Removes all bookmark faces overlays that are accumulating in the Org mode buffer that's open and used for capture."
    (interactive)
    (remove-overlays nil nil 'face 'bookmark-face))
  
;;;;; Jump to last capture
  (defun bookmark-jump-if-exists (bookmark-name)
    "Jumps to bookmark with name BOOKMARK-NAME if it exists in `bookmark-alist'."
    (when (cl-find-if (lambda (b) (string-equal (car b) bookmark-name)) bookmark-alist)
      (bookmark-jump bookmark-name)))
  
  (defun org-jump-to-last-capture ()
    "Jumps to last org capture bookmark"
    (interactive)
    (bookmark-jump-if-exists "org-capture-last-stored"))
  
;;;; Initialization of Org-mode  
  :init (progn
          (add-hook 'org-src-mode-hook
                    (lambda ()
                      (if (eq major-mode 'emacs-lisp-mode)
                          (flycheck-disable-checker 'emacs-lisp-checkdoc))))
          (add-hook 'org-mode-hook #'visual-line-mode)          
          ;; this will allow to insert org-tempo templates without annoying pair > inserted
          (add-hook 'org-mode-hook
                    (lambda ()
                      (setq-local electric-pair-inhibit-predicate
                                  `(lambda (c)
                                     (if (char-equal c ?<) t
                                       (,electric-pair-inhibit-predicate c))))))
          
          ;; remove overlays from the org-file
          (add-hook 'org-clock-goto-hook #'ffe-reset-bookmark-faces))
;;;; Org-mode related bindings global and local 
  :bind (:map ctl-z-map
              ;; global shortcuts 
              ("a" . org-agenda)
              ("l" . org-store-link)
              ("b" . org-switchb)
              ("c" . org-capture)
              ("j" . org-clock-goto)
              ("o" . org-jump-to-last-capture)
              :map org-mode-map
              ("C-c k" . org-cut-subtree)
              ("C-c C-x s" . ffe-org-insert-screenshot)
              ("M-n" . outline-next-visible-heading)
              ("M-p" . outline-previous-visible-heading)
              :map org-babel-map
              ("C-]" . ffy-org-goto-block-begin/end)
              ;;  Swap C-j and RET
              ([remap org-return-indent] . org-return)
              ([remap org-return] . org-return-indent)))

;;;; Org Journal
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-prefix-key "C-z S-"
        org-journal-file-type 'weekly
        org-journal-file-format "%Y%m%d_W%V.org"
        org-journal-enable-agenda-integration t
        org-journal-dir (file-name-as-directory
                         (concat (file-name-as-directory org-directory)
                                 "journal")))
  :bind (:map ctl-z-map
              ("n" . org-journal-new-entry)))

;;; Ledger
(use-package ledger-mode
  :ensure t
  :custom
  (ledger-binary-path "ledger")
  (ledger-clear-whole-transactions t)
  (ledger-default-date-format ledger-iso-date-format)
  (ledger-reports
   '(("monthly cf" "ledger -n --monthly [[ledger-mode-flags]] -f %(ledger-file)  reg Expenses or Liabilities:Mortgage or Income")
     ("unknown" "%(binary) [[ledger-mode-flags]] -f journal.ledger reg Expenses:Unknown")
     ("unknown-buffer" "%(binary) [[ledger-mode-flags]] -f journal.ledger --period %(buffer-year) reg Expenses:Unknown")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f journal.ledger reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :init
  (defun ffe-ledger-buffer-year-format-specifier()
    "This returns a year based on the buffer name"
    (f-base (file-name-parent-directory (buffer-file-name))))
  (defun ffe-ledger-current-year-format-specifier()
    (with-current-buffer (or ledger-report-buffer-name (current-buffer))
      (let* ((month (or ledger-report-current-month (ledger-report--current-month)))
           (year (car month)))
      (format "%s" year))))
  
  :config
  (add-to-list 'ledger-report-format-specifiers '("buffer-year" . ffe-ledger-buffer-year-format-specifier))
  (add-to-list 'ledger-report-format-specifiers '("current-year" . ffe-ledger-current-year-format-specifier))
  :defer t)

(use-package flycheck-ledger
  :ensure t
  :after ledger-mode)

;; (use-package hledger-mode
;;   :ensure t
;;   :custom
;;   (hledger-jfile "journal.ledger")
;;   :config
;;   (add-to-list 'company-backends 'hledger-company)
;;   (add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode)))


;; (use-package flycheck-hledger
;;   :ensure t)



;;; Docker
(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode
  :mode "Dockerfile")

(use-package docker
  :ensure t
  :pin melpa-stable
  :commands (docker-containers)
  :init (progn
	  (use-package json-rpc :ensure t)))

;;; Loading System Specific Files
;; Problem with Mac is that it sometime does or does not add ".local" to the system name as returned by
;; `system-name'. Here we try to mitigate this by adding ".local" and testing if that file exists. 
(when-let ((settings-file-base (concat *dotfiles-dir* (file-name-as-directory  "systems") (system-name)))
           (settings-file (concat settings-file-base ".el"))
           (settings-file-local (concat settings-file-base ".local.el"))
	       (exists (or (file-exists-p settings-file) (file-exists-p settings-file-local))))
  ;; load up the one that exists  
  (load (cl-some (lambda (a) (when (file-exists-p a) a)) (list settings-file settings-file-local))))

;;; Server Mode
;; Start server-mode if we are not in the daemon mode
(use-package server
  :config
  (progn 
    (unless (or (daemonp) (server-running-p))
      (server-mode 1))))


;;; Performance
;; How Long It Took
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading Emacs...done (%.3fs)" elapsed))

;; Local Variables:
;; eval: (outline-minor-mode t)
;; End:
