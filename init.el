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

;;;_. Autoloads file if it's present
(load *autoload-file* 'noerror)

;;;_. ELPA packages

;;;_ , Generating autoloads file from the installed packages
(defun ffy-find-package-autoloads-file (package)
  "Finds autoload file for the ELPA package and returns the path to the file, with a  check for file existence if performed, or nil.
One of the examples of using the 

 (ffy-find-package-autoloads-file 'haskell-mode)"
  ;; get version of the package version list to string 
  (let* ((pkg-str (if (symbolp package) (symbol-name package) package))
         (pkg-ver-str (mapconcat 'number-to-string 
                                 (elt (cdr  (assq package package-alist)) 0) "."))
         (pkg-al-file (concat (package--dir pkg-str pkg-ver-str) "/" pkg-str "-autoloads.el")))
    ;; check file existence 
    (when (file-exists-p pkg-al-file)
      pkg-al-file)))

(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

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

(add-hook 'kill-emacs-hook 'extract-autoloads)

;;;_ , Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            (funcall package-filter-function
                     (car package)
                     (package-desc-vers (cdr package))
                     archive))
    ad-do-it))

;;;_ , Add requiring package from ELPA (install if not installed)
(defun require-package (package &optional min-version)
  "Installs package of desired version using ELPA"
  ;; TODO: make it work with minimum and maximum version
  ;; use version-list-= and version-list-> from subr.el
  (unless (package-installed-p package min-version)
    (package-install package))
  (let ((pkg-autoload (ffy-find-package-autoloads-file package)))
    (when pkg-autoload
      (load pkg-autoload t))))

;;;_ , ELPA packages loaded
(when (require 'package nil 'noerror)
  ;; all ELPA packages are located here
  (setq package-user-dir (concat *dotfiles-dir* "elpa"))
;;;_  . Don't take Melpa versions of certain packages
  (setq package-filter-function
	(lambda (package version archive)
	  (or (not (string-equal archive "melpa"))
	      (not (memq package
			 '(
			   ruby-compilation
			   slime
			   color-theme-sanityinc-solarized
			   color-theme-sanityinc-tomorrow
			   elisp-slime-nav
			   findr))))))
;;;_  . Sources for the ELPA repositories 
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

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


;;;_ Key Bindings
;;; ----------------------------------------------------------------------
;;;_. Organization of key bindings
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


;;;_. Create Keymaps (ctl-x-f, ctl-z)
;;; ----------------------------------------------------------------------
(defvar ctl-x-f-map)
(define-prefix-command 'ctl-x-f-map)
(define-key global-map [(control x) ?f] 'ctl-x-f-map)
;;;; Borrowed this idea from http://www.jurta.org/en/emacs/dotemacs
;;; C-z ctl-z-map
;;; Make the prefix key `C-z' for my personal keymap.
;;; On qwerty-keyboards `C-z' is one of the most accessible keys
;;; like `C-x' and `C-c', but the prefix key `C-c' is reserved
;;; for mode-specific commands (both user-defined and standard Emacs extensions).
;;; The standard binding of `C-z' (`suspend-emacs' or `iconify-or-deiconify-frame')
;;; is reassigned here to double key sequence `C-z C-z'.
(defvar ctl-z-map
  (let ((map (make-sparse-keymap))
        (c-z (global-key-binding [(control ?z)])))
    (global-unset-key [(control ?z)])
    (define-key global-map [(control ?z)] map)
    (define-key map [(control ?z)] c-z)
    map))
;;; almost always hit suspend instead of repeat command
;;; so not repeat is moth C-x z and C-x C-z
(let ((c-x-z (global-key-binding [(control x) ?z])))
  (global-unset-key [(control x) (control ?z)])
  (define-key ctl-x-map [(control ?z)] c-x-z))


;;;_ Customizing General Emacs Behavior
;;; ----------------------------------------------------------------------
(require-package 'dash)
(autoload '-difference "dash")
(require-package 's)
(autoload 's-lines "s")

;;;_. GUI/Look and Feel
;;; ----------------------------------------------------------------------
;;;_ , adding packages from ELPA
(require-package 'idle-highlight-mode)
(require-package 'rainbow-mode)
(require-package 'rainbow-delimiters)
(require-package 'diminish)
(require-package 'powerline)
(require-package 'base16-theme)
(require-package 'minimap)

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
(when (require-package 'highlight-symbol)
  (highlight-symbol-mode +1)
  (define-key ctl-z-map [(control return)] 'highlight-symbol-at-point)
  (define-key ctl-z-map [(control up)] 'highlight-symbol-prev)
  (define-key ctl-z-map [(control down)] 'highlight-symbol-next)
  (define-key ctl-z-map [(@)] 'highlight-symbol-query-replace))

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
(when (fboundp 'powerline-default-theme)
  (powerline-default-theme))

;;;_ , UI Key-bindings
;;; ----------------------------------------------------------------------
;;; Turn on the menu bar for exploring new modes
(define-key global-map [f1] 'menu-bar-mode)
(define-key global-map [(control f1)] 'imenu-add-menubar-index)


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

(define-key ctl-x-f-map [(shift ?r)]  'recentf-open-most-recent-file)
(define-key ctl-x-f-map [?o] 'ido-find-file-other-window)
(define-key ctl-x-f-map [?f] 'find-file-in-project)
(define-key ctl-x-f-map [?r] 'ido-choose-from-recentf)
(define-key ctl-x-f-map [(return)] 'find-file-at-point)

;;;_ , Dired
;;; Dired settings that proved useful
(setq dired-dwim-target t)              ; guess where to copy files

(add-hook 'dired-mode-hook
          '(lambda ()
              (define-key dired-mode-map [(shift ?w)] 'wdired-change-to-wdired-mode)))


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
(define-key global-map [(control x) (control b)] 'ibuffer)
;;; more direct approach
(define-key global-map [f12] 'kill-this-buffer)
;;; other useful combos:
;;; `C-x 4 0' - kill-buffer-and-window (works with current buffer
;;; only)
;;; `C-x 4 b' - ido open buffer other window

;;; Buffer operations in C-z map
(define-key ctl-z-map [?b ?y] 'bury-buffer)
(define-key ctl-z-map [?b ?r] 'revert-buffer)


;;;_. Kill-rings
(require-package 'browse-kill-ring)
(require-package 'kill-ring-search)
;;; we use kill-ring-search through ELPA, hence check if it is
;;; available first
(when (fboundp 'kill-ring-search)
  (define-key global-map [(control meta ?y)] 'kill-ring-search))
;;; browse kill ring is nice too and also might be unavailable
(when (fboundp 'browse-kill-ring)
  (browse-kill-ring-default-keybindings) ; advise M-y
  (define-key global-map [(control x) (control ?y)] 'browse-kill-ring))

;;;_. Enable useful disabled commands
(dolist (command '(narrow-to-region narrow-to-defun narrow-to-page widen))
  (put command 'disabled nil))

;;;_. Undo settings
;;; ----------------------------------------------------------------------
(require-package 'undo-tree)
(when (fboundp 'global-undo-tree-mode)
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
(define-key global-map [(control h) ?a] 'apropos)

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
(eval-after-load "grep"
  '(progn
     (setq wgrep-enable-key "e")
     (define-key grep-mode-map [(?e)] 'wgrep-change-to-wgrep-mode)))

;;;_. Minibuffer and Smex
;;; ----------------------------------------------------------------------
(require-package 'smex)
(when (fboundp 'smex-initialize)
  (smex-initialize)
  ;; Smex is used in minibuffer M-x
  (define-key global-map [(meta ?x)] 'smex)
  (define-key global-map [(meta shift ?x)] 'smex-major-mode-commands))

;;; We are trying to make keys working in both Windows and Mac OS X
;;; To be able to M-x without meta
(define-key global-map [(control x) (control ?m)] 'execute-extended-command)


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
(require-package 'bm)
(require-package 'bookmark+)
(eval-after-load "bookmark"
  '(require 'bookmark+ nil t))

;;;_. Yasnippets
;;; ----------------------------------------------------------------------
(require-package 'yasnippet)

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


;;;_. Add ThingAtPoint+
(eval-after-load "thingatpt"
  '(when (require 'thingatpt+)
     (tap-redefine-std-fns)))

;;;_. Version Control Systems
;;;_ , Git
(require-package 'magit)
;;; Added global shortcut to run Magit
(when (fboundp 'magit-status)
  (define-key global-map [(control x) ?g] 'magit-status))

;;;_ Customizing Modes

;;;_. AllOut customizations
(eval-after-load "allout"
  '(progn
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
                           ("[?l]" allout-show-current-brunches)))
       (add-to-list 'allout-prefixed-keybindings keybinding))))

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

;;;_. HTML and XHTML setup

;;;_. Org Mode

; '(org-agenda-include-diary t)
; '(org-clock-clocktable-default-properties (quote (:maxlevel 4 :scope file :step week :block thisweek :tend "")))
; '(org-empty-line-terminates-plain-lists t)
; '(org-hide-leading-stars t)
; '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-id org-info org-jsinfo org-special-blocks org-w3m org-panel)))
; '(org-return-follows-link t)
; '(org-use-sub-superscripts (quote {}))

(setq org-completion-use-ido t
      org-hide-leading-stars t
      org-return-follows-link t
      org-modules '(org-docview org-gnus org-id org-info org-jsinfo org-protocol org-special-blocks org-w3m org-bookmark org-elisp-symbol org-panel)
      org-empty-line-terminates-plain-lists t)


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
  (define-key mode-specific-map [(control ?&)] 'org-mark-ring-goto)
  (define-key global-map [(control ?c) ?l] 'org-store-link)
  (define-key global-map [(control ?c) ?a] 'org-agenda)
  (define-key global-map [(control ?c) ?b] 'org-iswitchb))




;;;_. Markdown
(require-package 'markdown-mode)
;;; Add Markdown mode specifics.
;;; make outline-mode navigation work for underline headers as well
(add-hook 'markdown-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "#+\\|^\\(.*\\)\n\\(===+\\|---+\\)$")))

;;;_. General Programming Mode

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

(defface prog-mode-watchword-face
  '((((background light)) (:foreground "Red" :bold t))
    (((background dark)) (:foreground "Orange" :bold t)))
  "Highlight words like TODO, FIXME and so on"
  :group 'faces)

(defface prog-mode-bugs-face
  '((((background light)) (:background "Red" :foreground "Yellow"))
    (((background dark)) (:background "Red" :foreground "Yellow")))
  "Highlight Bug 1234 and alike"
  :group 'faces)

(defun prog-mode-faces-add ()
  "Highlights certain words, like BUG or XXX or FIXME in the code"
  (font-lock-add-keywords
   nil
   '(("\\s<\\s-*\\(FIX\\(ME\\)?\\|TODO\\|XXX\\):?\\(.+\\)\\>" 1 'prog-mode-watchword-face t)
     ("\\s<\\s-*\\([Bb][Uu][Gg]\\s-+[0-9]+\\)\\s-*:?"  1 'prog-mode-bugs-face t))))

(defun turn-on-flymake ()
  (flymake-mode))

(defun turn-on-electric-mode ()
  (electric-pair-mode +1))


(dolist   (it '(local-column-number-mode
                local-comment-auto-fill
                turn-on-hl-line-mode
                pretty-greek
                prog-mode-faces-add
                turn-on-flyspell-prog-mode
                turn-on-flymake))
  (if (fboundp it)
      (add-hook '*programming-hook* it)))

(defun ffy-run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))

;;;_. Auto-Complete

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *data-dir* "ac-dict"))
(setq ac-comphist-file (concat *data-dir* "ac-comphist.dat"))
(ac-config-default)
(setq ac-dwim t
      ac-auto-start t)

;;;_. Lisp-like Programming Languages
(require-package 'elisp-slime-nav)

(defconst *emacs-lisp-modes* '(emacs-lisp-mode lisp-mode ielm-mode))
(defconst *lisp-modes* (cons 'clojure-mode *emacs-lisp-modes*))

;;;_ , Paredit settings
(require-package 'paredit)

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [(control shift ?d)] (lambda () (paredit-forward-delete +1)))))

(defun ffy-init-lisp-minibuffer-enable-paredit-mode ()
  "Enable function `paredit-mode' during `eval-expression'. Adding `paredit-mode' for an `eval-expression' in minibuffer. RET  works as an exit minibuffer with evaluation."
  (if (eq this-command 'eval-expression)
      (when (fboundp 'paredit-mode)
        (paredit-mode +1))))

(add-hook 'minibuffer-setup-hook 'ffy-init-lisp-minibuffer-enable-paredit-mode)

;;;_ , Emacs Lisps
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

(defun ffy-setup-ielm ()
  "Sets some IELM defaults and keys."
  (interactive)
  (progn
    (local-set-key [return] 'ffy-ielm-return)))

(add-hook 'ielm-mode-hook 'ffy-setup-ielm)

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

(define-key global-map [(control ?c) (meta ?:)] 'ffy-ielm)

;;;_ , All Lisps
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
       (define-key lisp-mode-shared-map [(meta return)] 'reindent-then-newline-and-indent)
       (define-key lisp-mode-shared-map [(control x) ?x] 'eval-print-last-sexp)))

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
(require-package 'rinari)
(require-package 'rspec-mode)
(require-package 'ruby-compilation)
(require-package 'ruby-electric)
(require-package 'rvm)
(require-package 'yari)
(require-package 'flymake-ruby)

(dolist (pattern '("\\.rb$" "\\.rake$" "Rakefile$" "\\.gemspec$" "Gemfile$" "Capfile"))
  (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(global-rinari-mode 1)

(defun ffy-insert-ruby-string-interpolation ()
  "In a double quoted string, interpolation is inserted on #."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (inf-ruby-setup-keybindings)
     (define-key ruby-mode-map [(return)] 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map [(?#)] 'ffy-insert-ruby-string-interpolation)
     (define-key ruby-mode-map [(control ?h) ?r] 'yari)))

(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ffy-run-programming-hook)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;;_. Coffee-Script
;;; loading ELPA package
(require-package 'flymake-coffee)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

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


;;;_ Miscellaneous Keybindings
;;;_. Windows Operations
;;; ------------------------------------------------------------
;;;_ , Windmove
(windmove-default-keybindings 'super) ;; ⌘+direction
;;;_ , Moving in a window
(define-key goto-map [(?t)] (make-interactive move-to-window-line 0))
(define-key goto-map [(?b)] (make-interactive move-to-window-line -1))
;;;_ , Typical window operations but faster
(define-key global-map [(meta ?0)] 'delete-window)
(define-key global-map [(meta ?1)] 'delete-other-windows)
(define-key global-map [(meta ?2)] 'split-window-vertically)
(define-key global-map [(meta ?3)] 'split-window-horizontally)
;;;_ , Windows configurations
(define-key global-map [(control x) (super left)] 'winner-undo)
(define-key global-map [(control x) (super right)] 'winner-redo)

;;; ------------------------------------------------------------
;;;_. Editing/Operations In Buffer
;;; ------------------------------------------------------------
;;; Completion operations
(define-key global-map [(meta /)] 'hippie-expand)

;;; toggles line  numbers in the buffer
(define-key global-map [(control shift ?l)] 'linum-mode)
(define-key global-map [(control shift ?r)] 'search-backward)
(define-key global-map [(control shift ?s)] 'search-forward)

;;; M-z is zap-to-char now
(define-key global-map [(control meta ?z)]
    (lambda (char)
    (interactive "cZap to char backwards: ")
    (zap-to-char -1 char)))
(define-key global-map [(meta shift ?z)] 'zap-up-to-char)
(define-key global-map [(control meta shift ?z)]
  (lambda (char)
    (interactive "cZap up to char backwards: ")
    (zap-up-to-char -1 char)))

(defun ffy-bol-or-back-to-indent ()
  "In addition to having two different mappings for (move-beginning-of-line ARG) and (back-to-indentation) we will have a function that goes to BOL if we are on the indent position and to the indent if we are at the BOL"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))
;;; redefine C-a to C-S-a and C-a to the ffy-bol-or-back-to-indent
(define-key global-map [(control shift ?a)] (key-binding [(control ?a)]))
(define-key global-map [(control ?a)] 'ffy-bol-or-back-to-indent)

;;; use C-\ to leave one space between words
(define-key global-map [(control ?\\)] 'just-one-space)

(define-key global-map [(f5)] 'revert-buffer)
;;;
;;; Mark/Point machinery
;;; see
;;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
;;;
;;; pushes mark into a ring without activating a region
(define-key global-map [(meta ?\ )]
  (make-interactive (lambda ()
                      (push-mark (point) t nil)
                      (message "Position %s pushed to the ring" (point)))))

(eval-after-load "thingatpt"
  '(progn
     (define-key global-map [(control ?-)] 'ffy-tap-number-decrease)
     (define-key global-map [(control ?+)] 'ffy-tap-number-increase)))

;;; there's default M-^ `delete-indentation' that is an alias to join-line
(define-key ctl-z-map [(?j)] 'join-line)
(define-key ctl-z-map [(?J)] (lambda () "joins next line to this one"
                               (interactive)
                               (join-line 1)))
;;; ------------------------------------------------------------
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
;; End:
