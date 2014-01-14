;;;
;;; Emacs Configuration
;;;
;;;
;;; Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;;
;;; ----------------------------------------------------------------------

;;;_ Initialization
;;; debug if there's an error during loading
;(setq debug-on-error t)
(setq message-log-max 10000)
;;; benchmark time
(defconst *emacs-start-time* (current-time))
;;; it is hard to do anything without common-lisp
(eval-when-compile
  (require 'cl))

;;;_ Paths configuration
;;;_. Constants for paths
(defconst *dotfiles-dir*
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")
(defconst *site-lisp*
  (file-name-as-directory (concat *dotfiles-dir* "site-lisp"))
  "Directory for Emacs Extensions files")
(defconst *elpa-dir*
  (file-name-as-directory (concat *dotfiles-dir* "elpa"))
  "Directory for ELPA packages")
(defconst *data-dir*
  (file-name-as-directory (concat *dotfiles-dir* "data"))
  "Directory for miscellaneous data, s.a. backups, histories and caches")
(defconst *backup-dir*
  (file-name-as-directory (concat *data-dir* "backups"))
  "Directory for backups")
(defconst *autoload-file*
  (concat *dotfiles-dir* "loaddefs.el")
  "This is file containing all autoloads extracted from Emacs lisp files")
;;;_. Adding paths to `load-path'
;;; adding dot files to the load path
(add-to-list 'load-path *dotfiles-dir*)
;;; add recursively all subdirectories of *site-lisp*
;;; using temporary recursive function so not to clutter function
;;; space
(cl-labels ((add-directory-to-path (dir)
                                   (add-to-list 'load-path dir)
                                   (dolist (entry (directory-files-and-attributes dir))
                                     (if (and (cadr entry) ; t for directory
                                              (not (member (car entry) '("." "..")))) ; we don't want to deal with . and ..
                                         (let ((new-directory (expand-file-name (car entry) dir)))
                                           (add-to-list 'load-path new-directory)
                                           (add-directory-to-path new-directory))))))
  (add-directory-to-path *site-lisp*))

;;;_ Libraries and Packages

;;;_. built-in Emacs libraries
;;; We are trying to explicitly load as few libraries as possible.
(mapc #'require '(uniquify saveplace))

;;;_. packages in site-lisp
(mapc #'require '(use-package bind-key))


;;;_. Autoloads file if it's present
(load *autoload-file* 'noerror)

;;;_. ELPA packages

;;;_ , Generating autoloads file from the installed packages
;;;_  . close-autoloads advice
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;;;_  . extract-autoloads
(defun extract-autoloads ()
  "Extract autoloads recursively from *SITE-LISP* and puts it into *AUTOLOAD-FILE*"
  (interactive "f")
  (let* ((generated-autoload-file *autoload-file*)
         (buffer-file-coding-system 'no-conversion)
         ;; avoid generating autoloads for slime - results in error 
         ;; "Local variables entry is missing the suffix"
         (dir-list (loop for d in (directory-files *site-lisp* 'full "[^\(^\\.+$\|^slime\)]")
                         if (file-directory-p d)
                         collect d)))
    (apply 'update-directory-autoloads dir-list)))

;;;_  . Extract autoloads on killing Emacs
(add-hook 'kill-emacs-hook 'extract-autoloads)

;;;_ , Add requiring package from ELPA (install if not installed)
(defun require-package (package &optional min-version)
  "Installs package of desired version using ELPA"
  ;; TODO: make it work with minimum and maximum version
  ;; use version-list-= and version-list-> from subr.el
  (if (package-installed-p package min-version)
    t
    (package-install package)))

;;;_ , ELPA settings
(when (require 'package nil 'noerror)
  ;; all ELPA packages are located here
  (setq package-user-dir (concat *dotfiles-dir* "elpa"))
;;;_  . Sources for the ELPA repositories 
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

;;;_ Key Bindings Setup
;;; ----------------------------------------------------------------------
;;;_. Description of Organization of Key Bindings
;;;
;;; C-x primary map (some defaults)
;;; C-c secondary map (modes use it)
;;; C-z tertiary map (private custom one)
;;;
;;; Other maps:
;;;   C-x f  - map  file operations
;;;   M-g    - goto map (already exists in Emacs24)

;;; TODO: change M-o from facemenu-keymap to outline-mode keymap
;;;   M-o

;;;   C-<capital letter>
;;;   M-<capital letter>
;;;
;;;   A-<anything>
;;;   M-A-<anything>
;;;
;;; Single-letter bindings still available:
;;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;;   M- ?#


;;;_. Create Additional Keymaps (ctl-x-f, ctl-z)
;;; ----------------------------------------------------------------------
(defvar ctl-x-f-map)
(define-prefix-command 'ctl-x-f-map)

(bind-key "C-x f" 'ctl-x-f-map)

;;;; Borrowed this idea from http://www.jurta.org/en/emacs/dotemacs
;;; C-z ctl-z-map
;;; Make the prefix key `C-z' for my personal keymap.
;;; On qwerty-keyboards `C-z' is one of the most accessible keys
;;; like `C-x' and `C-c', but the prefix key `C-c' is reserved
;;; for mode-specific commands (both user-defined and standard Emacs extensions).
;;; The standard binding of `C-z' (`suspend-emacs' or `iconify-or-deiconify-frame')
;;; is reassigned here to double key sequence `C-z C-z'.
(defvar ctl-z-map)
(define-prefix-command 'ctl-z-map)
(let ((c-z (global-key-binding [(control ?z)])))
  (global-unset-key [(control ?z)])
  (bind-key "C-z" 'ctl-z-map)
  (bind-key "C-z C-z" c-z))

;;; almost always hit suspend instead of repeat command
;;; so `repeat' is both C-x z and C-x C-z
(let ((c-x-z (global-key-binding [(control x) ?z])))
  (global-unset-key [(control x) (control ?z)])
  (define-key ctl-x-map [(control ?z)] c-x-z))


;;;_ Utility functions

;;;_. eval-and-replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;;_. with-library
(defmacro with-library (symbol &rest body) 
  "A typical use is: you want to load a
 library, and then bind some function from that library to a key. But
 you don't want to attempt the binding if the library wasn't present,
 because that will throw an error, or something. This is particularly
 handy when you use the same .emacs on different machines, not all of
 which have the same libraries available.
 Example:
     (with-library tabbar
       (tabbar-mode)
       (global-set-key [(control tab)]       'tabbar-forward)
       (global-set-key [(control shift tab)] 'tabbar-backward)
       (global-set-key [(control shift iso-lefttab)] 'tabbar-backward)
       (global-set-key [(control f10)] 'tabbar-local-mode))

 In this example, if the library tabbar isn't available, Emacs will
 simply put a message in the echo area to that effect, and wonÃ¢_Tt even
 try to call global-set-key." 
  `(condition-case nil 
       (progn 
         (require ',symbol)
         ,@body) 
     (error 
      (message (format "%s is not available." ',symbol))
      nil)))

;;;_. on-win32
(defmacro on-win32 (&rest body)
  "Leaves code that specifically targets win32 system"
  `(when (equal system-type 'windows-nt)
     ,@body))

;;;_. on-mac
(defmacro on-mac (&rest body)
  "Leaves code that specifically targets Mac OS X"
  `(when (equal system-type 'darwin)
     ,@body))

;;;_. make-interactive
(defmacro make-interactive (func &rest args)
  "Returns a symbol of an anonymous interactive function, suitable for binding to keys."
  `(lambda ()
     (interactive)
     (,func ,@args)))

;;;_. pretty-greek
(defun pretty-greek () 
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon"
                 "phi" "chi" "psi" "omega"))) 
    (loop for word in greek for code = 97 then (+ 1 code) do  
          (let ((greek-char (make-char 'greek-iso8859-7 code))) 
            (font-lock-add-keywords 
             nil
             `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]") 
                (0 
                 (progn 
                   (decompose-region (match-beginning 2) 
                                     (match-end 2))
                   nil))))) 
            (font-lock-add-keywords 
             nil
             `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]") 
                (0 
                 (progn 
                   (compose-region (match-beginning 2) 
                                   (match-end 2) ,greek-char)
                   nil)))))))))  

;;;_. ido-choose-from-recentf
(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;;;_. ffy-display-prev-next-buffers
(defun ffy-display-prev-next-buffers ()
  "Show two previous, current and two next buffer names in the echo area.
Example:
-2:*Messages* -1:*Help*    0:.emacs      1:*info*  2:*scratch*

From http://www.jurta.org/en/emacs/dotemacs"
  (interactive)
  (let ((i -3) b (bl (buffer-list (selected-frame))) (message-log-max nil))
    (message "%s"
             (mapconcat
              (lambda (x)
                (setq i (+ i 1))
                (format "%d:%-12s"
                        i (substring (buffer-name x) 0
                                     (min (length (buffer-name x)) 11))))
              (append
               (nreverse
                (list
                 (setq b (get-next-valid-buffer (reverse bl) t))
                 (get-next-valid-buffer (cdr (memq b (reverse bl))) t)))
               (list (current-buffer))
               (list
                (setq b (get-next-valid-buffer (cdr bl) t))
                (get-next-valid-buffer (cdr (memq b bl)) t)))
              " "))))

;;;_. Load ThingAtPoint+ library
(eval-after-load "thingatpt"
  '(when (require-package 'thingatpt+)
     (tap-redefine-std-fns)))

;;;_. ffy-tap-number-change
;;; This depends on the thingatpt and thingatpt+
(defun ffy-tap-number-change (&optional num)
  "Changes the number at the point by `num' passed as a prefix argument. If no argument is passed then it uses 1, i.e. decrements and increments number at the point. If it is not a number at the point, then nothing happens."
  (interactive "p")
  (save-excursion
    (let ((n (tap-number-at-point-decimal))
          (bounds (tap-bounds-of-number-at-point)))
      (if (and n bounds)
          (progn
            (delete-region (car bounds) (cdr bounds))
            (insert (number-to-string (+ n (or num 1)))))))))

;;;_. ffy-tap-number-decrease
(defun ffy-tap-number-decrease (&optional num)
  "Decreases number at the point by `num' or 1 if argument is not given"
  (interactive "p")
  (ffy-tap-number-change (- (or num 1))))

;;;_. ffy-tap-number-increase
(defun ffy-tap-number-increase (&optional num)
  "Increases number at the point by `num' or 1 if argument is not given"
  (interactive "p")
  (ffy-tap-number-change (or num 1)))

;;;_. assocs
(defun assocs (keylist list)
  "like `assoc' but KEYLIST is a list of keys. Returns a subset of alist LIST with keys from KEYLIST"
  (mapcar (lambda (k) (assoc k list)) keylist))

;;;_. *frame-original-geometry*
(defvar *frame-original-geometry* nil "Original width, height, left and top of the frame")

;;;_. ffy-save-original-frame-parameters
(defun ffy-save-original-frame-parameters ()
  "Writes original frame geometry parameters into a variable to be restored later."
  (setq *frame-original-geometry*
    (assocs '(width height top left fullscreen) (frame-parameters))))

(add-hook 'after-init-hook 'ffy-save-original-frame-parameters)

;;;_. ffy-frame-maximize
(defun ffy-frame-maximize ()
  (on-mac
   (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
  ;; on Windows we send a WM message to the window
  ;; to maximize Emacs frame
  (on-win32
   (w32-send-sys-command #xf030 (selected-frame))))

;;;_. ffy-frame-originalize
(defun ffy-frame-originalize ()
  (mapc (lambda (param)
          (set-frame-parameter (selected-frame) (car param) (cdr param)))
        *frame-original-geometry*)
  ;; on Windows in addition to the frame parameters we send WM message
  ;; to the system window to
  ;; restore original size of the Emacs frame
  (on-win32
   (w32-send-sys-command #xf120 nil)))


;;;_. add-to-hooks
(defmacro add-to-hooks (hooks func)
  "Adds FUNC to HOOKS"
  `(dolist (hook ,hooks)
     (add-hook hook ,func)))

;;;_. font-lock-add-buffer-keywords
(defun font-lock-add-buffer-keywords (keywords &optional append)
  "Add highlighting KEYWORDS for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument APPEND is `set', they are used to replace the current
highlighting list.  If APPEND is any other non-nil value, they are added at the
end of the current highlighting list.

For example:

 (font-lock-add-buffer-keywords
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns: one to fontify `FIXME:' words, even in
comments, and the other to fontify `and', `or' and `not' words as keywords.

Note that some modes have specialised support for additional patterns, e.g.,
see the variables `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'."
  ;; This is needed to avoid this operation ending up as a no-op (because
  ;; `font-lock-set-defaults' might get called later, and it might decide to
  ;; set `font-lock-keywords' itself, from scratch).  I understand that some
  ;; older FSF Emacs releases don't do this in `font-lock-add-keywords', so
  ;; we do it here -- which can't hurt.
  (font-lock-set-defaults)
  ;; Use a native implementation if one exists
  (if (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords nil keywords append)
    ;; Otherwise, use this one that was grabbed from FSF Emacs 21's
    ;; `font-lock-add-keywords' and `font-lock-remove-keywords' functions.
    (if (eq append 'set)
        (setq font-lock-keywords keywords)
      ;; Try to remove duplicates
      (setq font-lock-keywords (copy-sequence font-lock-keywords))
      (dolist (kw keywords)
        (setq font-lock-keywords
              (delete kw
                      ;; The keywords might be compiled
                      (delete (font-lock-compile-keyword kw)
                              font-lock-keywords))))
      (let ((old font-lock-keywords))
        (when (eq (car-safe font-lock-keywords) t)
          (pop old))
        (when append
          (rotatef keywords old))
        (setq font-lock-keywords (append keywords old))))))


;;;_ Customizing General Emacs Behavior
;;; ----------------------------------------------------------------------
(require-package 'dash)
(autoload '-difference "dash")
(require-package 's)
(autoload 's-lines "s")

;;;_. GUI/Look and Feel
;;; ----------------------------------------------------------------------
;;;_ , adding packages from ELPA
(use-package idle-highlight-mode
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package diminish
  :ensure t)
(use-package base16-theme
  :ensure t)
(use-package minimap
  :ensure t)

;;;_ , Turn off some bells and whistles
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;_ , File name into the frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;;;_ , highlight the "word" the cursor is on
(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (highlight-symbol-mode +1)
    (bind-key "<C-return>" 'highlight-symbol-at-point  ctl-z-map)
    (bind-key "<C-up>" 'highlight-symbol-prev  ctl-z-map)
    (bind-key "<C-down>" 'highlight-symbol-next  ctl-z-map)
    (bind-key "@" 'highlight-symbol-query-replace  ctl-z-map)))

;;;_ , display time in mode-line
(display-time)

;;;_ , Hide some modes from the mode-line
;;; TODO: maybe this should be moved to the corresponding modes configuration
(when (fboundp 'diminish)
  (eval-after-load 'eldoc
    '(diminish 'eldoc-mode))
  (eval-after-load 'undo-tree
    '(diminish 'undo-tree-mode)))

;;;_ , Configure powerline if it is available
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;;;_ , UI Key-bindings
;;; ----------------------------------------------------------------------
;;; Turn on the menu bar for exploring new modes
(bind-key "<f1>" 'menu-bar-mode)
(bind-key "<C-f1>" 'imenu-add-menubar-index)


;;;_. Files Settings and Operations
;;; ----------------------------------------------------------------------

;;;_ , Backups and saves
(setq save-place-file (concat *data-dir* "places")
      backup-directory-alist `((".*" . ,*backup-dir*))
      savehist-file (concat *data-dir* "history")
      smex-save-file (concat *data-dir* ".smex-items")
      recentf-save-file (concat *data-dir* ".recentf")
      ido-save-directory-list-file (concat *data-dir* ".ido.last")
      bookmark-default-file (concat *data-dir* "bookmarks")
      desktop-dirname *data-dir*
      desktop-path (list desktop-dirname)
      desktop-save t
      auto-save-list-file-prefix (concat *data-dir* "auto-save-list/.saves-"))

;;;_ , Files and Projects
(require-package 'find-file-in-project)

;;;_ , Files Key-Bindings
;;; ----------------------------------------------------------------------
;;;  C-x C-f is bound to ido-find-file
;;;
;;;  C-x f <letter> are different file commands

(bind-key "R" 'recentf-open-most-recent-file ctl-x-f-map)
(bind-key "o" 'ido-find-file-other-window ctl-x-f-map)
(bind-key "f" 'find-file-in-project ctl-x-f-map)
(bind-key "r" 'ido-choose-from-recentf ctl-x-f-map)
(bind-key "RET" 'find-file-at-point ctl-x-f-map)

;;;_ , Dired
;;; Dired settings that proved useful
(setq dired-dwim-target t)              ; guess where to copy files

(add-hook 'dired-mode-hook
          '(lambda ()
              (bind-key "W" 'wdired-change-to-wdired-mode dired-mode-map)))


;;;_. Byte Compilation
;; --------------------------------------------------
(add-hook 'after-save-hook (lambda ()
                             (when (eq major-mode 'emacs-lisp-mode)
                               (emacs-lisp-byte-compile-and-load))))


;;;_. Buffers
;;; ----------------------------------------------------------------------
;; Encoding and text related stuff
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)


(set-default 'imenu-auto-rescan t)

(defadvice previous-buffer (after my/previous-buffer activate)
  (ffy-display-prev-next-buffers))

(defadvice next-buffer (after my/next-buffer activate)
 (ffy-display-prev-next-buffers))

;;; Zap-up-to char is a better alternative
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)" 'interactive)


(defvar *auto-close-buffers* '("*Completions*"
                               "*Ido Completions*")
  "List of buffers that should be closed after we done with minibuffer. Usually it is various completions buffers")

(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (progn
               (mapc '(lambda (buffer)
                        (if (buffer-live-p buffer)
                            (kill-buffer buffer))) *auto-close-buffers*))))

;;;_ , Buffer Operations Keybindings
;;; ------------------------------------------------------------
(bind-key "C-x C-b" 'ibuffer)
;;; more direct approach
(bind-key "<f12>" 'kill-this-buffer)
;;; other useful combos:
;;; `C-x 4 0' - kill-buffer-and-window (works with current buffer
;;; only)
;;; `C-x 4 b' - ido open buffer other window

;;; Buffer operations in C-z map
(bind-key "b y" 'bury-buffer  ctl-z-map)
(bind-key "b r" 'revert-buffer  ctl-z-map)


;;;_. Kill-rings
(use-package browse-kill-ring
  :ensure t
  :config
  (progn
    (browse-kill-ring-default-keybindings) ; advise M-y
    (bind-key "C-x C-y" 'browse-kill-ring)))

(use-package kill-ring-search
  :ensure t
  :config
  (progn
    (bind-key "C-M-y" 'kill-ring-search)))

;;;_. Enable useful disabled commands
(dolist (command '(narrow-to-region narrow-to-defun narrow-to-page widen))
  (put command 'disabled nil))

;;;_. Undo settings
;;; ----------------------------------------------------------------------
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;;_. Spellcheck setup 
;;; ------------------------------------------------------------
;;;_ , find-hunspell-dictionary
(defun find-hunspell-dictionary ()
  "Searches for hunspell dictionaries using `hunspell -D' first and seeing if ther's
any dictionary found. If not then try to check if dictionary exist in the same directory (case for Windows).

On Mac OS X hunspell should search for dictionaries in at least /Library/Spelling. In fact, on Windows just drop dictionaries next to hunspell binary file.

It returns either nil or path to the dictionary that could be used with `hunspell -d'. Put it in the `ispell-extra-args' variable.

This function depends on 's and 'dash libraries."
  (when (executable-find "hunspell")
    ;; First, let's see if we can load any dicts by default
    (let* ((hunspell-output (shell-command-to-string "hunspell -D"))
           (hunspell-output-lines (remove-if #'(lambda (e) (equal e ""))
                                             (s-lines hunspell-output)))
           (loaded-dicts (member "LOADED DICTIONARY:"  hunspell-output-lines))
           (available-dicts (-difference (member-if #'(lambda (e)(s-starts-with? "AVAILABLE DICTIONARIES" e)) hunspell-output-lines)
                                         loaded-dicts)))
      ;; If we have loaded-dicts we should be fine, otherwise try to
      ;; search for dictionaries
      (unless
          (or (cdr loaded-dicts)
              ;; Could be a message:
              ;; Can't open affix or dictionary files for dictionary named
              ;; "default".
              (not (cdr available-dicts)))
        ;; let's check if there's dictionary next to the binary
        (let ((dictionary-path (concat
                                (file-name-directory
                                 (executable-find "hunspell")) "en_US")))
          (when (file-exists-p (concat dictionary-path ".dic"))
            dictionary-path))))))

;;;_ , loading iSpell
(eval-after-load "ispell"
  '(progn
     ;; Personal dictionary setup
     ;; if file doesn't exist then create it
     (setq ispell-personal-dictionary (let ((personal-dictionary-file (concat *data-dir* ".personal.dict")))
                                        (unless (file-exists-p personal-dictionary-file)
                                          (with-temp-file personal-dictionary-file t))
                                        personal-dictionary-file))
     ;; Aspell Specific
     (when (executable-find "aspell")
       (setq ispell-program-name "aspell"
             ispell-extra-args '("--sug-mode=ultra")))
     ;; Hunspell Specific
     (when (executable-find "hunspell")
       (setq ispell-program-name "hunspell")
       (let* ((dict-location (find-hunspell-dictionary)))
         (when dict-location
           (setq  ispell-extra-args '("-d" dict-location "-i" "utf-8")))))))

;;;_. Help and Info
;;; ----------------------------------------------------------------------
(require 'help-mode+ nil t)
(require 'help+ nil t)
(require 'help-fns+ nil t)
;;; apropos seems to be more useful than apropos-command
(bind-key "C-h a" 'apropos)

;;;_. Miscellaneous
;;; ----------------------------------------------------------------------
(setq redisplay-dont-pause t)
(defalias 'yes-or-no-p 'y-or-n-p)
(random t)
(put 'set-goal-column 'disabled nil)

;;;_. Ack/Grep/RGrep
;;; ----------------------------------------------------------------------
(require-package 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(use-package grep
  :defer t
  :config
  (progn
    (setq wgrep-enable-key "e")
    (bind-key "e" 'wgrep-change-to-wgrep-mode  grep-mode-map)))

;;;_. Minibuffer and Smex
;;; ----------------------------------------------------------------------
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  ;; Smex is used in minibuffer M-x
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;;; We are trying to make keys working in both Windows and Mac OS X
;;; To be able to M-x without meta
(bind-key "C-x C-m"  'execute-extended-command)


;;;_. Using smerge for merging files
;;; ----------------------------------------------------------------------
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;;;_. Bookmarking
;;; ----------------------------------------------------------------------
(use-package bm
  :ensure bm)

(use-package bookmark
  :defer t
  :config
  (progn
    (use-package bookmark+
      :ensure t)))

;;;_. Yasnippets
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (progn
    (use-package dropdown-list
      :ensure t)
    (add-to-list 'yas-snippet-dirs (concat *data-dir*  "snippets"))
    (yas-global-mode +1)))

;;;_. Ido configuraiton
;;; Some IDO settings that have been taken out from the customization file.
;;; ----------------------------------------------------------------------
(require-package 'ido-yes-or-no)
(require-package 'ido-ubiquitous)

(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
;;; not every command should could be ido-ed
;;; kill-ring-search has already set of minibuffer commands that don't
;;; work well with ido-completing-read
(setq ido-ubiquitous-command-exceptions '(kill-ring-search))
;;;_ , ffy--change-ido-override
(defun ffy--change-ido-override (behavior func-name)
  "Changes `ido-ubiquitous-function-overrides` variable for a function FUNC-NAME by setting its behavior to BEHAVIOR"
  (setq ido-ubiquitous-function-overrides
         (mapcar (lambda (override) (if  (equal (caddr override) ,func-name)
                                   (cons ,behavior (cdr override))
                                 override))
                 ido-ubiquitous-function-overrides)))
;;;_ , enable-ido-for
(defmacro enable-ido-for (func-name)
  "Enables IDO for a function using `ido-ubiquitous' mode"
  `(ffy--change-ido-override 'enable ,func-name))
;;;_ , disable-ido-for
(defmacro disable-ido-for (func-name)
  "Disables IDO for a function using `ido-ubiquitous' mode"
  `(ffy--change-ido-override 'disable ,func-name))

;;;_. Version Control Systems
;;;_ , Git
(require-package 'magit)
;;; Added global shortcut to run Magit
(when (fboundp 'magit-status)
  (bind-key "C-x g"  'magit-status))

;;;_. Auto-Complete
(require-package 'popup)
(require-package 'fuzzy)
(require-package 'auto-complete)
(require 'popup)
(require 'fuzzy)
(require 'auto-complete)
(require 'auto-complete-config)

;;; add our own directory to the end of the list
(add-to-list 'ac-dictionary-directories (concat *data-dir* "ac-dict") t)
(setq ac-comphist-file (concat *data-dir* "ac-comphist.dat"))
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
;(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(
               ac-source-abbrev
               ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-yasnippet
               ))

;;; FIX: fixing issue with ac-prefix-symbol with thingatpt+
;;; If bounds-of-thing-at-point has been redefined (and we did so)
;;; this function will return nil. 
(defun ac-prefix-symbol ()
  "Overriden default prefix definition function."
  (let ((symbol-start (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (and (null symbol-start)
             (fboundp 'tap-bounds-of-thing-nearest-point))
        ;; try tap- function if available
        (car-safe (tap-bounds-of-thing-nearest-point 'symbol))
      ;; else
      symbol-start)))

;;;_ Customizing Modes

;;;_. AllOut customizations
(use-package allout
  :config
  (progn
    (setq
     ;; we want to keep C-k as paredit and other
     ;; useful modes for editing
     allout-unprefixed-keybindings nil)
    ;; add missing but useful keybindings. in allout mode
    ;; it is set through changin allout-prefixed-keybindings
    (dolist (keybinding '(("[(shift ?h)]" allout-hide-bodies)
                          ("[(shift ?l)]" allout-hide-current-leaves)
                          ("[(shift ?e)]" allout-hide-current-entry)
                          ("[?e]" allout-show-entry)
                          ("[?o]" allout-show-to-offshoot)
                          ("[?b]" allout-show-current-branches)))
      (add-to-list 'allout-prefixed-keybindings keybinding))
    ))

;;;_. XSL/XML setup.
(defun xml-pretty-print (begin end)
  "Makes current buffer with XML markup look prettier"
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (xml-pretty-print begin end))

(defun xml-pretty-print-buffer ()
  "Formats whole buffer containing XML"
  (interactive)
  (xml-pretty-print-region (point-min) (point-max)))

(setq-default
 ;; Treat elements and contents like S-expressions! Oh, the magic. 
 ;; (if you know S-expression movement commands, it's great) 
 nxml-sexp-element-flag t
  ;; Whenever you type </ it will fill out the rest. 
 nxml-slash-auto-complete-flag t)


;; Causes files with extensions .xml .xsl .rng .xhtml .html and .tal
;; to invoke nxml-mode.
(setq auto-mode-alist 
      (cons '("\\.\\(xml\\|xsl\\|rng\\|tal\\|xsd\\|sch\\|xslt\\|svg\\|rss\\)\\'" . nxml-mode) 
            (remove-if (lambda (x) (eq (cdr x) 'html-mode)) auto-mode-alist)))

;; another way to recognize XML files 
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(defun ffy-customize-nxml-mode ()
  "This function sets some variables and calls some functions that setup nXML mode."
  ;; load hide show modes 
  (local-set-key "\C-c/" 'nxml-finish-element)
  (local-set-key [return] 'newline-and-indent)
  ;;(auto-fill-mode)
  (rng-validate-mode)
  (unify-8859-on-decoding-mode)
  (setq ispell-skip-html t)
  (hs-minor-mode 1)
  ;; controversial 
  (make-variable-buffer-local 'ido-use-filename-at-point)
  (setq ido-use-filename-at-point nil))

(add-hook 'nxml-mode-hook 'ffy-customize-nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
               ""
               nil))
;;; Add auto-complete to the the XML based modes 
(dolist (mode '(nxml-mode))
  (add-to-list 'ac-modes mode))

;;;_. HTML and XHTML and other markup mode setup setup
;; (dolist (mode '(html-mode yaml-mode  textile-mode))
;;   (add-to-list 'ac-modes mode)


;;;_. Org Mode
(setq org-completion-use-ido t
      ;; org-completion-use-iswitchb t     ; without it ido completion is
      ;;                                   ; not going to work for
      ;;                                   ; org-mode (see `org-read-property-value')
      org-hide-leading-stars t
      org-return-follows-link t
      org-modules '(org-docview org-gnus org-id org-info org-jsinfo org-protocol org-special-blocks org-w3m org-bookmark org-elisp-symbol org-panel)
      org-empty-line-terminates-plain-lists t)

;;; Override not working function from org-mode
(eval-after-load "org"
  '(defun org-read-property-value (property)
    "Read PROPERTY value from user."
    (let* ((completion-ignore-case t)
           (allowed (org-property-get-allowed-values nil property 'table))
           (cur (org-entry-get nil property))
           (prompt (concat property " value"
                           (if (and cur (string-match "\\S-" cur))
                               (concat " [" cur "]") "") ": "))
           (set-function (org-set-property-function property))
           (val (if allowed
                    (funcall set-function prompt allowed nil
                             (not (get-text-property 0 'org-unrestricted
                                                     (caar allowed))))
                  (funcall set-function prompt
                           (mapcar 'list (org-property-values property))
                           nil nil "" nil cur))))
      (if (equal val "")
          cur
        val))))

;(setq org-todo-keyword-faces
;      (quote (("TODO" :foreground "medium blue" :weight bold)
;              ("NOTE" :foreground "dark violet" :weight bold)
;              ("STARTED" :foreground "dark orange" :weight bold)
;              ("WAITING" :foreground "red" :weight bold)
;              ("DELEGATED" :foreground "red" :weight bold))))

;(defun my-org-mode-custom-bindings ()
;  "customize org-mode keys"
;  (local-set-key [(control up)] 'outline-previous-visible-heading)
;  (local-set-key [(control down)]  'outline-next-visible-heading)
;  (local-set-key [(control meta up)]  'outline-up-heading)
;  (local-set-key [(control c) (meta ?w)] 'org-store-link )
;  (local-set-key [(control c) (control ?y)] 'org-insert-link)
;  (local-set-key [(control c) ?a] 'org-agenda))


(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'yas-minor-mode-on)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'hl-line-mode)

(dolist (mode '(org-mode))
  (add-to-list 'ac-modes mode))

;;
;;  Setup iimage working with Org-mode
;; 
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))


;;;_ , Org-mode bindings
(when (fboundp 'org-mode)
  ;; due to the conflict with Yasnippet
  (bind-key "C-&" 'org-mark-ring-goto  mode-specific-map)
  (bind-key "C-c l" 'org-store-link)
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c b" 'org-iswitchb))


;;;_. Markdown
(use-package markdown-mode
  :config
  (progn
    (defun set-markdown-mode-outline-regexp ()
      "Add Markdown mode specifics.  Make outline-mode navigation work for underline headers as well"
      (make-local-variable 'outline-regexp)
      (setq outline-regexp "#+\\|^\\(.*\\)\n\\(===+\\|---+\\)$"))

    (add-hook 'markdown-mode-hook 'set-markdown-mode-outline-regexp)))

;;;_. General Programming Mode
(autoload 'turn-on-fic-mode "fic-mode")

(defvar *programming-hook* nil
  "This variable contains functions that we need to run if we are programming ")

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-flyspell-prog-mode ()
  (when (and (boundp 'ispell-program-name) 
             (executable-find ispell-program-name))
    (flyspell-prog-mode)))

;;;_ , Flymake and Syntax checking
(use-package flymake
  :init (progn
          (use-package flymake-cursor  :ensure t)
          (defun turn-on-flymake ()
            (flymake-mode))))

;;;_ , Electric and Autopairs
(defun turn-on-electric-mode ()
  (electric-pair-mode +1))

;;;_ , All Programming Modes would benefit from this
(dolist   (it '(local-column-number-mode
                local-comment-auto-fill
                turn-on-hl-line-mode
                pretty-greek
                turn-on-fic-mode
                turn-on-flyspell-prog-mode
                turn-on-flymake))
  (if (fboundp it)
      (add-hook '*programming-hook* it)))

(defun ffy-run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))

;;;_. Lisp-like Programming Languages
;;;_ , Paredit settings
(use-package paredit
  :ensure t
  :diminish (paredit-mode . "(P)")
  :config
  (progn
    (defun ffy-paredit-forward-delete ()
      "Forces deleting a character in ParEdit mode"
      (paredit-forward-delete +1))

    (bind-key "C-S-d" 'ffy-paredit-forward-delete  paredit-mode-map)

    (defun ffy-init-lisp-minibuffer-enable-paredit-mode ()
      "Enable function `paredit-mode' during `eval-expression'. Adding `paredit-mode' for an `eval-expression' in minibuffer. RET  works as an exit minibuffer with evaluation."
      (if (eq this-command 'eval-expression)
          (when (fboundp 'paredit-mode)
            (paredit-mode +1))))

    (add-hook 'minibuffer-setup-hook 'ffy-init-lisp-minibuffer-enable-paredit-mode)))

;;;_ , Emacs Lisps
(use-package elisp-slime-nav
  :ensure t
  :diminish t)

;;; modes that deal with EmacsLisp
(defconst *emacs-lisp-modes* '(emacs-lisp-mode lisp-mode ielm-mode))

(defun ffy-init-lisp-emacs-setup ()
  "Only emacs-lisp related things."
  (progn
    (make-local-variable 'hippie-expand-try-functions-list)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol 'to-the-end)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially 'to-the-end)
    (elisp-slime-nav-mode 1)
    (add-to-list 'ac-sources 'ac-source-emacs-lisp-features)))

(dolist (mode *emacs-lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-lisp-emacs-setup)))

;;;_  . IELM
;;;_   , ffy-ielm-return
(defun ffy-ielm-return ()
  "Like `ielm-return' but more intellectual when it comes to deciding when just
send `paredit-newline' instead.
Implementation shamelessly stolen from: https://github.com/jwiegley/dot-emacs/blob/master/init.el"
  (interactive)
  (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))
;;;_   , ffy-setup-ielm
(defun ffy-setup-ielm ()
  "Sets some IELM defaults and keys."
  (interactive)
  (progn
    (local-set-key [return] 'ffy-ielm-return)))
;;;_   , adding IELM setup hook
(add-hook 'ielm-mode-hook 'ffy-setup-ielm)
;;;_   , ffy-ielm
(defun ffy-ielm ()
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
;;;_   , adding keys (C-c M-:) to start IELM with current buffer
(bind-key "C-c M-:" 'ffy-ielm)

;;; add Auto-Complete to the IELM
(dolist (mode '(inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))

;;;_ , All Lisps
(defconst *lisp-modes* (cons 'clojure-mode *emacs-lisp-modes*))

(defun ffy-init-lisp-setup ()
  "This is the setup that would any lisp based mode benefit from"
  (progn
       (when (fboundp 'paredit-mode) 
         (paredit-mode +1))
       (turn-on-eldoc-mode)
       (ffy-run-programming-hook)
       (when (fboundp 'highlight-parentheses-mode)
         (highlight-parentheses-mode +1))
       (when (fboundp 'rainbow-delimiters-mode)
         (rainbow-delimiters-mode))
       (bind-key "<M-return>" 'reindent-then-newline-and-indent  lisp-mode-shared-map)
       (bind-key "C-x x" 'eval-print-last-sexp  lisp-mode-shared-map)))

(dolist (mode *lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-lisp-setup)))

;;;_ , Slime Settings
(eval-after-load "slime"
  '(progn
     (message "Check if slime has been loaded !")

     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-net-coding-system 'utf-8-unix)

     (slime-setup '(slime-fuzzy slime-banner slime-repl))

     (add-hook 'lisp-mode-hook
               (lambda ()
                 (slime-mode t)))

     (add-hook 'inferior-lisp-mode-hook
               (lambda ()
                 (inferior-slime-mode t)))

     (add-hook 'slime-repl-mode-hook
               (lambda ()
                 (when (fboundp 'paredit-mode)
                   (paredit-mode +1))
                 ;; set some keys to behave like we are in paredit
                 ;; prevent grabbing DEL button from
                 ;; http://www.emacswiki.org/emacs/ParEdit
                 (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)))
     ))




;;;_ , Clojure Mode Setup
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'nrepl)
(require-package 'ac-nrepl)

(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'clojure-test-mode)

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(eval-after-load "ac-nrepl"
  '(progn (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
          (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
          (add-to-list 'ac-modes 'nrepl-mode)))

(defun ffy-find-file-in-clojure-project ()
  "For Clojure we are also looking for project.clj file in the project root"
  (progn
    (require 'find-file-in-project)
    (when (boundp 'ffip-project-file)
      (set (make-local-variable 'ffip-project-file)
           (if (listp 'ffip-project-file)
               (cons "project.clj" ffip-project-file)
             (list "project.clj" ffip-project-file))))))

(add-hook 'clojure-mode-hook 'ffy-find-file-in-clojure-project)

;;;_. Ruby/Rails setup
;;; Loading  Ruby and Rails relate ELPA packages
(use-package ruby-mode
  :ensure t
  :init (progn
          (use-package rinari
            :ensure t
            :config
            (global-rinari-mode 1))
          (use-package rspec-mode :ensure t)
          (use-package ruby-compilation :ensure t)
          (use-package ruby-electric :ensure t)
          (use-package ruby-end
            :ensure t
            :config (progn
                      (defalias 'ruby-insert-end 'ruby-end-insert-end)))
          (use-package rvm :ensure t)
          (use-package yari :ensure t)
          (use-package flymake-ruby :ensure t)

          (defun ffy-insert-ruby-string-interpolation ()
            "In a double quoted string, interpolation is inserted on #."
            (interactive)
            (insert "#")
            (when (and
                   (looking-back "\".*")
                   (looking-at ".*\""))
              (insert "{}")
              (backward-char 1)))
          )
  :config (progn
            (inf-ruby-setup-keybindings)
            (bind-key "<return>" 'reindent-then-newline-and-indent ruby-mode-map)
            (bind-key "#" 'ffy-insert-ruby-string-interpolation  ruby-mode-map)
            (bind-key "C-h r" 'yari  ruby-mode-map)
            (add-hook 'ruby-mode-hook 'subword-mode)
            (add-hook 'ruby-mode-hook 'ruby-electric-mode)
            (add-hook 'ruby-mode-hook 'ffy-run-programming-hook)
            (add-hook 'ruby-mode-hook 'flymake-ruby-load))
  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode)))

;;;_. HAML/SCSS/SASS setup
;;; loading ELPA packages
(require-package 'flymake-haml)
(require-package 'flymake-sass)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'scss-mode-hook 'flymake-sass-load)
(add-hook 'haml-mode-hook 'flymake-haml-load)
(add-hook 'scss-mode-hook 'ffy-run-programming-hook)
(add-hook 'sass-mode-hook 'ffy-run-programming-hook)
;;; custom line opening
(defun ffy-open-line-indented (n)
  "like `open-line' but keeps indentation"
  (interactive "*p")
  (let* ((loc (point-marker)))
    (newline-and-indent)
    (goto-char loc)))

;;; my own customizations
(defun ffy-customize-sass-scss-mode ()
  (interactive)
  ;; first of all <ret> sets newline and indent as C-j
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(control return)] 'ffy-open-line-indented))

(add-hook 'scss-mode-hook  'ffy-customize-sass-scss-mode)
(add-hook 'sass-mode-hook  'ffy-customize-sass-scss-mode)

;;; add Auto-Complete HAML SCSS and SASS modes
(dolist (mode '(haml-mode sass-mode scss-mode))
  (add-to-list 'ac-modes mode))

;;;_. Coffee-Script
;;; loading ELPA package
(require-package 'flymake-coffee)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;;;_. JavaScript
;;; ----------------------------------------------------------------------
;;; Good link about setting up Javascript:
;;; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
;;;
;;; == Setup Rhino ==
;;; On Windows:
;;; 1. Download rhino1_7R4.zip from Mozilla page.
;;; 2. Unpack it and copy js.jar to %JRE_HOME%\lib\ext
;;;
;;; On OSX
;;; 1. Download rhino and unzip it
;;; 2. Make a the library directory if it doesn't exist:
;;;      mkdir -p ~/Library/Java/Extensions
;;; 3. Copy the jar to the extensions directory:
;;;      cp ~/Downloads/rhino1_7R2/js.jar ~/Library/Java/Extensions/
;;;
;;; == Test Rhino ==
;;;
;;; Run the following command:
;;;        java org.mozilla.javascript.tools.shell.Main
(use-package js2-mode
  :ensure t
  :diminish (js2-minor-mode . "JS2")
  :init  (progn
           (use-package js-comint
             :ensure t
             :config (progn
                       ;; Use NodeJS as our repl if it is available
                       ;; otherwise stick to the Rhino
                       (let* ((node-program (executable-find "node"))
                              (node-command (if node-program (concat node-program " --interactive")))
                              (js-command  (or node-command "java org.mozilla.javascript.tools.shell.Main")))
                         (setq inferior-js-program-command js-command))))

           (use-package flymake-jslint :ensure t)
           (use-package flymake-jshint :ensure t)
           (use-package ac-js2 :ensure t)
           (use-package js2-imenu-extras
             :config (js2-imenu-extras-setup))


           ;; TODO: Add Swank-js
           ;; http://www.idryman.org/blog/2013/03/23/installing-swank-dot-js/
           ;; Install Swank.js by
           ;;     npm install -g swank-js
           ;; Test by running
           ;;     swank-js
           ;; And directing browser to http://localhost:8009/swank-js/test.html

           (defun ffy-js-mode-customizations ()
             "JavaScript customizations"
             ;; Scan the file for nested code blocks
             (imenu-add-menubar-index)
             ;; Activate the folding mode
             (hs-minor-mode t))

           (add-to-list 'interpreter-mode-alist '("node" . js-mode))
           (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
           (add-to-list 'auto-mode-alist '("\\.json$" . js-mode)))

  :config (progn
            (add-hook 'js-mode-hook 'ffy-js-mode-customizations)
            (add-hook 'js-mode-hook 'js2-minor-mode)
            (add-hook 'js-mode-hook 'turn-on-electric-mode)
            (add-hook 'js-mode-hook 'turn-on-flymake)
            (add-hook 'js2-mode-hook 'ac-js2-mode)
            (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

            ;;; Add Auto-Complete to JavaScript modes.
            (dolist (mode '(espresso-mode js-mode js2-minor-mode js2-mode))
              (add-to-list 'ac-modes mode))))

;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list
;;          'comint-preoutput-filter-functions
;;          (lambda (output)
;;            (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list 'comint-preoutput-filter-functions
;;                      (lambda (output)
;;                        (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;;                      (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))

;;;_. Scala setup
(require-package 'scala-mode)

;;;_. Octave Mode

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook (lambda ()
                              (auto-fill-mode 1)))

;;;_. Haskell Mode
(require-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'ffy-run-programming-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;;;_ Key Bindings
;;;_. Windows Operations
;;; ------------------------------------------------------------
;;;_ , Windmove
(windmove-default-keybindings 'super) ;; ⌘+direction
;;;_ , Moving in a window
(bind-key "t" (make-interactive move-to-window-line 0)  goto-map)
(bind-key "b" (make-interactive move-to-window-line -1)  goto-map)
;;;_ , Typical window operations but faster
(bind-key "M-0" 'delete-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-vertically)
(bind-key "M-3" 'split-window-horizontally)
;;;_ , Windows configurations
(define-key global-map [(control x) (super left)] 'winner-undo)
(define-key global-map [(control x) (super right)] 'winner-redo)

;;; ------------------------------------------------------------
;;;_. Editing/Operations In Buffer
;;; ------------------------------------------------------------
;;;_ , Completion operations
(bind-key "M-/"  'hippie-expand)
;;;_ , toggles line  numbers in the buffer
(bind-key "C-S-l"  'linum-mode)
;;;_ , search forward/backward
(bind-key "C-S-r"  'search-backward)
(bind-key "C-S-s"  'search-forward)

;;;_ , Zapping backa and forth
;;;_  . zap-to-char-backwards
(defun zap-to-char-backwards (char)
    (interactive "cZap to char backwards: ")
    (zap-to-char -1 char))
;;;_  . zap-up-char-backwards
(defun zap-up-char-backwards (char)
    (interactive "cZap up to char backwards: ")
    (zap-up-to-char -1 char))
;;;_  . M-z is zap-to-char
(bind-key "C-M-z" 'zap-to-char-backwards)
(bind-key "M-Z"  'zap-up-to-char)
(bind-key "C-M-S-z" 'zap-up-char-backwards)

;;;_ , ffy-bol-or-back-to-indent
(defun ffy-bol-or-back-to-indent ()
  "In addition to having two different mappings for (move-beginning-of-line ARG) and (back-to-indentation) we will have a function that goes to BOL if we are on the indent position and to the indent if we are at the BOL"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))
;;;_ , redefine C-a to C-S-a and C-a to the ffy-bol-or-back-to-indent
(bind-key "C-S-a" (key-binding [(control ?a)]))
(bind-key "C-a"  'ffy-bol-or-back-to-indent)
;;;_ , use C-\ to leave one space between words
(define-key global-map [(control ?\\)] 'just-one-space)
;;;_ , update buffer with F5
(bind-key "<f5>" 'revert-buffer)
;;;_ , Mark/Point machinery

;;; see
;;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/

;;; pushes mark into a ring without activating a region
(bind-key  "M-SPC"
  (make-interactive (lambda ()
                      (push-mark (point) t nil)
                      (message "Position %s pushed to the ring" (point)))))
;;;_ , inc/dec number at the point
(eval-after-load "thingatpt"
  '(progn
     (bind-key "C--"  'ffy-tap-number-decrease)
     (bind-key "C-+"  'ffy-tap-number-increase)))
;;;_ , there's default M-^ `delete-indentation' that is an alias to join-line
(bind-key "j" 'join-line ctl-z-map)
(bind-key "J" (lambda () "joins next line to this one"
                               (interactive)
                               (join-line 1)) ctl-z-map)
;;;_ , mark commands from `thing-cmds'
(when  (require-package 'thing-cmds)
  (thgcmd-bind-keys))

;;;_. Outline mode
;;; ------------------------------------------------------------
;(define-key global-map [(meta ?o)] '...)

;;; ------------------------------------------------------------

;;;_ Start Server
(server-start)

;;;_ Custom variables and faces
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

;;;_ Loading machine specific settings
(let ((system-specific-config (concat *dotfiles-dir* system-name ".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;;;_ How long did it take to load
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading Emacs...done (%.3fs)" elapsed))




;; Local Variables:
;; allout-layout: t
;; byte-compile-warnings: (not cl-functions)
;; End:
