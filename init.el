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
	  (use-package ido-completing-read+
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
  :config (setq ffip-prefer-ido-mode t
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

;; we will use M-o for outline. Facemenu is super useless outside of center-* functions
(define-key global-map (kbd "M-o") nil)
(define-key global-map (kbd "C-z f") 'facemenu-keymap)

(use-package outshine
  :ensure t
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
;; This is a lisp based programming language configuration
;; mostly Emacs lisp and IELM and such
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

;; turn off checkdoc for my configuration files
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
         (let ((f (file-name-nondirectory (buffer-file-name))))
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
  
  :bind (("C-c C-c" . go-run-buffer)))


;;;; Javascript
;; Javascript Configuration
;;
;; Node packages expected to be installed globally:
;; 1. tern
;; 2. mocha
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

(use-package tern
  :commands (tern-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

;; We can't quite manipulate local mode keymap from within a hook, so we do it in EAL form
(eval-after-load "tern"
  '(progn
     ;; clear C-c C-r and C-c C-d from tern-keymap
     (define-key tern-mode-keymap (kbd  "C-c C-r") nil)
     (define-key tern-mode-keymap (kbd "C-c C-d") nil)
     (bind-keys :map tern-mode-keymap
                ("C-c C-r r" . tern-rename-variable)
                ("C-c C-d C-d" . tern-get-docs)
                ("C-c C-d d" . tern-get-type))))


(use-package company-tern
  :after company
  :ensure t
  :init (progn
          (defun ffe-js2-mode-company-hook ()
            (ffe-add-company-backends 'company-tern 'company-semantic))
          (add-hook 'js2-mode-hook #'ffe-js2-mode-company-hook)))

(use-package mocha
  :commands (mocha-test-at-point mocha-test-file mocha-test-project)
  :ensure t
  :pin melpa-stable
  :bind (:map js2-mode-map
              ("C-c t ." . mocha-test-at-point)
              ("C-c t f" . mocha-test-file)
              ("C-c t p" . mocha-test-project)))

(use-package js-comint
  :ensure t
  :commands (run-js))

;; customize projectile
(when (fboundp 'projectile-register-project-type)
  (projectile-register-project-type 'npm '("package.json") :test "npm test"  :test-suffix ".spec"))

(use-package tide
  :ensure t)

;;;; Python 
;; Python configuration
;; for Windows pyreadline needs to be installed
(use-package python
  :defer t
  :diminish (python-mode . "Py")
  :commands python-mode
  :init
  (progn
    (setq python-shell-interpreter "python"
          python-shell-prompt-block-regexp "\\s-*\\.\\.\\."))
  :config
  (progn
    (add-hook 'python-mode-hook #'eldoc-mode))
  :bind (:map python-mode-map
	      ;; python-eldoc-at-point is not really useful, instead
	      ;; use it for sending file to python shell
	      ("C-c C-f" . python-shell-send-file)
	      ("C-M-f" . python-nav-forward-sexp)
	      ;; moves between syntactic blocks (if,for,while,def,..)
	      ("C-M-b" . python-nav-backward-sexp)
	      ("M-}" . python-nav-forward-block)
	      ("M-{" . python-nav-backward-block)
	      ;; In python this looks like next line skipping comments and multi-line strings
	      ("M-e" . python-nav-forward-statement)
	      ("M-a" . python-nav-backward-statement)))
;;
;; If anaconda produces error like
;; TypeError: __init__() got an unexpected keyword argument 'environment'
;; see https://github.com/proofit404/anaconda-mode/issues/296
;; most likely issue is jedi < 0.12, check via `pip list`
;; install up to date version via `pip install -U jedi` and error goes away
(use-package anaconda-mode
  :defer t
  :ensure t
  :diminish (anaconda-mode . "Ana")
  :init (progn
	  (setq anaconda-mode-installation-directory (concat *data-dir* "anaconda-mode"))
          (add-hook 'python-mode-hook #'anaconda-mode)
          (add-hook 'python-mode-hook #'anaconda-eldoc-mode))
  :config (progn
            ;; This one will prevent annoying window from popping up
            ;; see https://github.com/proofit404/anaconda-mode/issues/164
            ;; and https://github.com/syl20bnr/spacemacs/issues/3772
            (remove-hook 'anaconda-mode-response-read-fail-hook
                         'anaconda-mode-show-unreadable-response))
  :bind (:map anaconda-mode-map
	      ;; make key bindings more traditional
	      ("M-," . anaconda-mode-go-back)
	      ("M-*" . anaconda-mode-find-assignments)
	      ("C-c C-d" . anaconda-mode-show-doc)))


(use-package company-anaconda
  :defer t
  :ensure t
  :init (with-eval-after-load 'company
          (add-hook 'python-mode-hook (lambda () (ffe-add-company-backends 'company-anaconda)))))




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

;;;; F#
(use-package fsharp-mode
  :defer t
  :ensure t)

;;; TeX Mode
;; TeX Settings
(use-package tex-site                   ; AucTeX initialization
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ; parse documents for auto completion
        TeX-auto-save  t                ; parse on save
        ))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))
        
(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t)

(use-package bibtex                     ; BibTeX editing
  :ensure auctex
  :defer t)

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'TeX-mode-hook #'reftex-mode))

;;;; Metapost Extra
(use-package meta-mode
  :defer t)

;;;; Preview Metapost Buffer via MPtoPDF
;;(use-package meta-mode-extra)

;;;; ConTexT Specifics
(use-package context
  :defer t
  :mode (("\\.mkiv\\'" . context-mode)
         ("\\.mkii\\'" . context-mode)))

;; The layout of the ConTeXt installation base is well-defined

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
  :mode (("\\.ya?ml$" . yaml-mode)))

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

;;;; Shell Scripts 
(use-package sh-script
  :mode (("\\.zsh" . sh-mode)))


;;; Org Mode

;; comment out ob-ipython as it gives error if no ipython is installed
;; (use-package ob-ipython
;;   :ensure t)

(use-package plantuml-mode
  :ensure t)

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
               (plantuml . t)))
            ;; add files to registers for a quick call
            (set-register ?i (cons 'file (concat org-directory "/Ideas.org")))

            ;; Refiling - allow creating new targets
            (setq org-refile-allow-creating-parent-nodes 'confirm)
            )
  :init (progn
          (add-hook 'org-src-mode-hook
                    (lambda ()
                      (if (eq major-mode 'emacs-lisp-mode)
                          (flycheck-disable-checker 'emacs-lisp-checkdoc))))
          (add-hook 'org-mode-hook
                    (lambda ()
                      ;; (add-hook 'completion-at-point-functions
                      ;;           #'pcomplete-completions-at-point)
                      )))
  :bind (:map ctl-z-map
              ("a" . org-agenda)
              ("l" . org-store-link)
              ("b" . org-switchb)
              ("c" . org-capture)
              :map org-mode-map
              ("C-c k" . org-cut-subtree)
              ("M-n" . outline-next-visible-heading)
              ("M-p" . outline-previous-visible-heading)
              ;;  Swap C-j and RET
              ([remap org-return-indent] . org-return)
              ([remap org-return] . org-return-indent)))

;;; Ledger
(use-package ledger-mode
  :ensure t)
(use-package flycheck-ledger
  :ensure t
  :after ledger-mode)

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

;; Local Variables:
;; eval: (outline-minor-mode t)
;; End:
