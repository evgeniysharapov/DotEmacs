;;; Constants and Paths
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

;;; Load Libraries Recursively
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

;;; Packages Repos and Use-Package
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
;;  Using `diminish'
(use-package diminish :ensure t)
(use-package subr-x)

;;;; Ido Configuration
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
	    (setq-default org-completion-use-ido t)))

;;; Keymap and Keys Organization 

;; put my own keymap on C-z while moving zap to M-z
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

;;; UI
;; Configuring User experience and UI
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
  (set-frame-font (ido-completing-read+ "Choose font:"
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

;;;; Windows
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
  :bind (:map ctl-x-w-map
              ("w" . ace-window)))

(defun toggle-window-split ()
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

;;;; Look and Feel
(use-package whitespace
  :bind (:map ctl-x-t-map
              ("SPC" . whitespace-mode)))

(use-package rainbow-mode
  :ensure t
  :bind (:map ctl-x-t-map
              ("r" . rainbow-mode)))

(use-package crosshairs
  :bind (:map ctl-x-t-map
              ("+" . crosshairs-mode)))

;; there's no point in hiding menubar on macos
(when (not (string= system-type "darwin"))
  (custom-set-minor-mode 'menu-bar-mode nil))
(custom-set-minor-mode 'tool-bar-mode nil)
(custom-set-minor-mode 'scroll-bar-mode nil)

(when (display-graphic-p)
  (custom-set-minor-mode 'mouse-wheel-mode t)
  (custom-set-minor-mode 'blink-cursor-mode nil))


;;; Files

;;;; Sessions
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

;;;; Project Files
(use-package find-file-in-project
  :ensure t
  :commands find-file-in-project
  :config (setq ffip-prefer-ido-mode t
                ffip-match-path-instead-of-filename t)
  :bind (:map ctl-x-f-map
              ("f" . find-file-in-project)))

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

  (defun ido-find-recent-file ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (unless recentf-mode
      (recentf-mode t))
    (if (find-file (ido-completing-read+ "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))
  
  :config
  (progn
    (add-to-list 'recentf-exclude
	         (expand-file-name *data-dir*))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

  :bind (:map ctl-x-f-map
              ("r" . ido-find-recent-file)
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
  :init (progn
	  (use-package imenu+ :defer t)
	  (use-package imenu-list :ensure t :commands (imenu-list)))
  :bind (:map search-map
         ("i" . imenu)
	 ("I" . imenu-list)))

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


;;;; Visibility

;; Visibility of the text in a window
;; C-z /          hides lines matching regexp
;; C-u C-z /      hides lines not matching regexp
;; C-u C-u C-z /  unhides everything
(use-package hide-lines
  :defer t
  :bind (:map ctl-z-map
	      ("/" . hide-lines)))

;;;; OnScreen
;; moving onto anything that is visible on the screen
(use-package ace-jump-mode
  :ensure t
  :bind (:map goto-map
	 ("j" . ace-jump-mode)))

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
  :bind (("M-s a" . ag)
	 ("M-s p" . ag-project)))

(use-package wgrep-ag :ensure t :defer t)

;;;; Rg

;;;; Isearch
;; These are handy keys when you are navigating a buffer using isearch
(use-package isearch
  :config (setq isearch-allow-scroll t)
  :bind (:map isearch-mode-map
              ("<up>" . isearch-repeat-backward)
              ("<down>" . isearch-repeat-forward)))

;;; Edit
;; Editing Operations
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
  :config (global-undo-tree-mode))

(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :init 
  (use-package browse-kill-ring+
    :defer 10)
  :bind (("C-M-y" . browse-kill-ring)))

;; TODO activate it later
(use-package visual-regexp
  :disabled t
  :ensure t
  :bind (("C-c C-/" . vr/replace)))



;;; Buffers
;; Buffer operations
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

(defun ffe-kill-current-buffer ()
  "Kills current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(bind-key "C-x K" #'ffe-kill-current-buffer)

;;; Spellcheck
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

;;; Help
;; Help/Info configuration
;;;; Emacs Help
(use-package help-mode+)
(use-package help+)
(use-package help-fns+)

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
  :bind (:map help-help-map
              ("f" . helpful-function)
              ("v" . helpful-variable)
              ("c" . helpful-command)
              ("m" . helpful-macro)
              ("h" . helpful-at-point)))


;;;; Info

;;; Tweaks
;; short response function instead of long one
(fset 'yes-or-no-p 'y-or-n-p)

;;; Calendar
(use-package calendar
  :config
  (setq diary-file (concat *data-dir* "diary")))

;;; Completion in Buffer
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
	      ("g" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package monky
  :ensure t
  :commands monky-status
  :bind (:map ctl-z-map
	      ("h" . monky-status)))

;;; Minibuffer
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


;;; Programming Modes

;;;; General Settings 
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

;;;; Lisp
(use-package ffe-lisp)

;;;; Clojure
(use-package ffe-clojure)

;;;; C/C++
(use-package ffe-c
  :commands ffe-c-mode-hook
  :init (add-hook 'c-mode-common-hook #'ffe-c-mode-hook))

;;;; Go
(use-package ffe-go)

;;;; Javascript
(use-package ffe-javascript
  :after projectile)			; because we customize projectile for javascript

;;;; Python 
(use-package ffe-python)

;;;; Rust
(use-package ffe-rust)

;;;; Groovy
(use-package groovy-mode
  :ensure t)

;;; TeX Mode
(use-package ffe-tex)


;;; Misc File Formats

;;;; JSON
(use-package ffe-json)

;;;; NGinx Configuration
(use-package nginx-mode
  :ensure t
  :commands nginx-mode)

;;;; YAML
(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode)))

;;;; Markdown
(use-package markdown-mode  
  :ensure t
  :mode (("\\.md$" . markdown-mode))
  :config (add-hook 'markdown-mode-hook #'flyspell-mode)

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

;;;; Shell Scripts 
(use-package sh-script
  :mode (("\\.zsh" . sh-mode)))


;;; Org Mode
(use-package ffe-org)

;;; Docker
(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)
(use-package docker
  :ensure t
  :pin melpa-stable
  :commands (docker-containers)
  :init (progn
	  (use-package json-rpc :ensure t)))


;;; Loading System Specific Files
(when-let ((local-settings (concat *dotfiles-dir* (system-name) ".el"))
	   (exists (file-exists-p local-settings)))
  (load local-settings))

;;; Load custom-vars File
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

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
(put 'narrow-to-region 'disabled nil)
