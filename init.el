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

;;; We use some of the CL functions for its convinience
;;; `cl-labels', `cl-remove-if' and `cl-loop' 
;;; TODO: think about combining it with loading emacs' built-ins
(require 'cl-lib)

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
(defconst *system-specific*
  (file-name-as-directory (concat *dotfiles-dir* "systems"))
  "Directory with system specific initialization commands")

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
;;;_. Emacs built-ins
;;; We would need these throughout the config file
;;; We are trying to explicitly load as few libraries as possible.
(mapc #'require '(uniquify saveplace))
;;;_. *SITE-LISP* packages
;;; our primary library loader is `use-package'. If it can't be loaded
;;; we should run:
;;;
;;;     $ git submodule update --init
;;;
;;; from the ~/.emacs.d directory
(unless (require 'use-package nil 'noerror)
   (let ((default-directory (file-name-directory load-file-name)))
 	 (shell-command "git submodule update --init"))
   (message "Updated use-package libraries"))
(mapc #'require '(use-package bind-key))
;;;_. ELPA packages
;;;_ , Generating autoloads file from the installed packages
;;;_  . close-autoloads advice
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))
;;;_ , ELPA settings
(when (require 'package nil 'noerror)
  ;; all ELPA packages are located here
  (setq package-user-dir (concat *dotfiles-dir* "elpa"))
  ;; Sources for the ELPA repositories 
  (setq package-archives
        '(("gnu"         . "http://elpa.gnu.org/packages/")
          ("org"         . "http://orgmode.org/elpa/")
          ("melpa"       . "http://melpa.milkbox.net/packages/")
          ("marmalade"   . "http://marmalade-repo.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))


;;;_. Autoloads file
;;;_ , Extract autoloads on killing Emacs
(defun extract-autoloads ()
  "Extract autoloads recursively from *SITE-LISP* and puts it into *AUTOLOAD-FILE*"
  (interactive "f")
  (let* ((generated-autoload-file *autoload-file*)
         (buffer-file-coding-system 'no-conversion)
         ;; avoid generating autoloads for slime - results in error 
         ;; "Local variables entry is missing the suffix"
         (dir-list (cl-loop for d in (directory-files *site-lisp* 'full "[^\(^\\.+$\|^slime\)]")
                            if (file-directory-p d)
                            collect d)))
    (apply 'update-directory-autoloads dir-list)))
(add-hook 'kill-emacs-hook 'extract-autoloads)
;;;_ , Load autoloading file if it is present
(load *autoload-file* 'noerror)

;;;_ Extend Emacs Functionality
;;;_. Additional Libraries
(use-package dash
  :ensure t
  :commands -difference)

(use-package s
  :ensure t
  :commands s-lines)


;;;_. Utility functions

;;;_ , eval-and-replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;;_ , with-library
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

;;;_ , on-win32
(defmacro on-win32 (&rest body)
  "Leaves code that specifically targets win32 system"
  `(when (equal system-type 'windows-nt)
     ,@body))

;;;_ , on-mac
(defmacro on-mac (&rest body)
  "Leaves code that specifically targets Mac OS X"
  `(when (equal system-type 'darwin)
     ,@body))

;;;_ , make-interactive
(defmacro make-interactive (func &rest args)
  "Returns a symbol of an anonymous interactive function, suitable for binding to keys."
  `(lambda ()
     (interactive)
     (,func ,@args)))

;;;_ , pretty-greek
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

;;;_ , ido-choose-from-recentf
(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;;;_ , ffy-display-prev-next-buffers
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

;;;_ , assocs
(defun assocs (keylist list)
  "like `assoc' but KEYLIST is a list of keys. Returns a subset of alist LIST with keys from KEYLIST"
  (mapcar (lambda (k) (assoc k list)) keylist))

;;;_ , *frame-original-geometry*
(defvar *frame-original-geometry* nil "Original width, height, left and top of the frame")

;;;_ , ffy-save-original-frame-parameters
(defun ffy-save-original-frame-parameters ()
  "Writes original frame geometry parameters into a variable to be restored later."
  (setq *frame-original-geometry*
    (assocs '(width height top left fullscreen) (frame-parameters))))

(add-hook 'after-init-hook 'ffy-save-original-frame-parameters)

;;;_ , ffy-frame-maximize
(defun ffy-frame-maximize ()
  (on-mac
   (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
  ;; on Windows we send a WM message to the window
  ;; to maximize Emacs frame
  (on-win32
   (w32-send-sys-command #xf030 (selected-frame))))

;;;_ , ffy-frame-originalize
(defun ffy-frame-originalize ()
  (mapc (lambda (param)
          (set-frame-parameter (selected-frame) (car param) (cdr param)))
        *frame-original-geometry*)
  ;; on Windows in addition to the frame parameters we send WM message
  ;; to the system window to
  ;; restore original size of the Emacs frame
  (on-win32
   (w32-send-sys-command #xf120 nil)))


;;;_ , add-to-hooks
(defmacro add-to-hooks (hooks func)
  "Adds FUNC to HOOKS"
  `(dolist (hook ,hooks)
     (add-hook hook ,func)))

;;;_ , font-lock-add-buffer-keywords
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


;;;_ , keymap-on-key macro
(defmacro keymap-on-key (name keys)
  "This is a macro that declares a variable, key prefix and assigns a key to it.
NAME is symbol of the new keymap and KEYS is a string that represents keys as for macro `kbd'"
  `(progn (defvar ,name)
          (define-prefix-command (quote ,name))
          (bind-key ,keys (quote ,name))))

;;;_ , set-keymap-prefix
;; (defun set-keymap-prefix (keymap keys)
;;   "Remaps whole KEYMAP to the given KEYS. In other words it will add a key prefix to
;; all mappings in a given KEYMAP.
;; KEYS is the argument acceptable by `kbd' or `read-kbd-macro' macro"
;;   ;; Idea is to create a temporary keymap with the given prefix add
;;   ;; all the bindingds from the given keymap and then assign given
;;   ;; keymap temporary keymap
;;   ;; (let ((tmp-keymap (copy-keymap )))
;;   ;;   (map-keymap (lambda (k v) ()))
;;   ;;   )
;;   ;; (define-key global-map [(control ?`)] 'flyspell-mode-map)
;;   )

;;;_ , insert-date-and-time
(defun insert-date-and-time ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%d/%m/%Y, %k:%M" (current-time))))

;;;_ Key Bindings Setup
;;; ----------------------------------------------------------------------
;;;_. Description of Organization of Key Bindings
;;;
;;; C-x primary map (some defaults)
;;; C-c secondary map (modes use it)
;;; C-z tertiary map (private custom one)

;;; The sequence continues with quaternary, quinary, senary,
;;; septenary, octonary, nonary, and denary, although most of these
;;; terms are rarely used. There's no word relating to the number
;;; eleven but there is one that relates to the number twelve:
;;; duodenary.

;;; C-' quaternary map

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

;;; Usually following keys are easy to press with one hand
;;; M-f12, M-f11, M-f10, M-f9, M-f8, M-f7

;;;_. Create Additional Keymaps (ctl-x-f, ctl-z and ctl-quote)
(keymap-on-key ctl-x-f-map "C-x f")

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

(keymap-on-key ctl-quote-map "C-'")


;;;_ User Interface
;;;_. adding packages from ELPA
;(use-package base16-theme :ensure t :defer t)
;(use-package minimap  :ensure t :defer t)

;;;_. Turn off some bells and whistles
;;; turn off menu bar, scroll bars and tool bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;_. File name into the frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;;;_. display time in mode-line
(display-time)

;;;_. Hide some modes from the mode-line
;;; TODO: maybe this should be moved to the corresponding modes configuration
(when (fboundp 'diminish)
  (eval-after-load 'eldoc
    '(diminish 'eldoc-mode)))

;;;_. Mode line configuration
(use-package diminish  :ensure t :defer t)
(use-package powerline
  :ensure t
  :config (progn
            (defun ffy-powerline-theme ()
              "Powerline setup for the mode-line."
              (interactive)
              (setq-default mode-line-format
                            '("%e"
                              (:eval
                               (let* ((active (powerline-selected-window-active))
                                      (mode-line (if active 'mode-line 'mode-line-inactive))
                                      (face1 (if active 'powerline-active1 'powerline-inactive1))
                                      (face2 (if active 'powerline-active2 'powerline-inactive2))
                                      (separator-left (intern (format "powerline-%s-%s"
                                                                      powerline-default-separator
                                                                      (car powerline-default-separator-dir))))
                                      (separator-right (intern (format "powerline-%s-%s"
                                                                       powerline-default-separator
                                                                       (cdr powerline-default-separator-dir))))
                                      (lhs (list (powerline-raw "%*" nil 'l)
                                                 (powerline-buffer-size nil 'l)
                                                 (powerline-raw mode-line-mule-info nil 'l)
                                                 (powerline-buffer-id nil 'l)
                                                 (when (and (boundp 'which-func-mode) which-func-mode)
                                                   (powerline-raw which-func-format nil 'l))
                                                 (powerline-raw " ")
                                                 (funcall separator-left mode-line face1)
                                                 (when (boundp 'erc-modified-channels-object)
                                                   (powerline-raw erc-modified-channels-object face1 'l))
                                                 (powerline-major-mode face1 'l)
                                                 (powerline-process face1)
                                                 (powerline-minor-modes face1 'l)
                                                 (powerline-narrow face1 'l)
                                                 (powerline-raw " " face1)
                                                 (funcall separator-left face1 face2)
                                                 (powerline-vc face2 'r)))
                                      (rhs (list (powerline-raw global-mode-string face2 'r)
                                                 (funcall separator-left face2 face1)
                                                 (powerline-raw "%4l" face1 'l)
                                                 (powerline-raw ":" face1 'l)
                                                 (powerline-raw "%3c" face1 'r)
                                                 (funcall separator-right face1 face2)
                                                 (powerline-raw " ")
                                                 (powerline-raw "%6p" nil 'r)
                                                 (powerline-hud face2 face1))))
                                 (concat (powerline-render lhs)
                                         (powerline-fill face2 (powerline-width rhs))
                                         (powerline-render rhs)))))))

           (ffy-powerline-theme)))

;;;_. Menu bar
;;; Turn on the menu bar for exploring new modes
(bind-key "<f1>" 'menu-bar-mode)
(bind-key "<C-f1>" 'imenu-add-menubar-index)


;;;_ Files/Directories
;;;_. Backups and saves
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

;;;_. Files and Projects
(use-package find-file-in-project
  :ensure t
  :commands find-file-in-project)

;;;_. Files Key-Bindings
;;;  C-x C-f is bound to ido-find-file
;;;
;;;  C-x f <letter> are different file commands

(bind-key  "R"   'recentf-open-most-recent-file ctl-x-f-map)
(bind-key  "o"   'ido-find-file-other-window    ctl-x-f-map)
(bind-key  "f"   'find-file-in-project          ctl-x-f-map)
(bind-key  "r"   'ido-choose-from-recentf       ctl-x-f-map)
(bind-key  "RET" 'find-file-at-point            ctl-x-f-map)

;;;_. Dired
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


;;;_ Buffers
;;;_. show adjacent buffers in the minibuffer on switch
(defadvice previous-buffer (after my/previous-buffer activate)
  (ffy-display-prev-next-buffers))

(defadvice next-buffer (after my/next-buffer activate)
 (ffy-display-prev-next-buffers))

(defvar *auto-close-buffers* '("*Completions*"
                               "*Ido Completions*")
  "List of buffers that should be closed after we done with minibuffer. Usually it is various completions buffers")

(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (progn
               (mapc '(lambda (buffer)
                        (if (buffer-live-p buffer)
                            (kill-buffer buffer))) *auto-close-buffers*))))


(bind-key "C-x C-b" 'ibuffer)
(bind-key "<f12>" 'kill-this-buffer)

;;; other useful combos:
;;; `C-x 4 0' - kill-buffer-and-window (works with current buffer
;;; only)
;;; `C-x 4 b' - ido open buffer other window

;;; Buffer operations in C-z map
(bind-key "b y" 'bury-buffer  ctl-z-map)
(bind-key "b r" 'revert-buffer  ctl-z-map)
;;; revert buffer on f5
(bind-key "<f5>" 'revert-buffer)



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
           (hunspell-output-lines (cl-remove-if #'(lambda (e) (equal e ""))
                                                (s-lines hunspell-output)))
           (loaded-dicts (member "LOADED DICTIONARY:"  hunspell-output-lines))
           (available-dicts (-difference (cl-member-if #'(lambda (e)(s-starts-with? "AVAILABLE DICTIONARIES" e)) hunspell-output-lines)
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
(use-package ispell
  :config (progn
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

;;;_ Windows
;;;_. Windmove
(windmove-default-keybindings 'super) ;; ⌘+direction
;;;_. Moving in a window
(bind-key "t" (make-interactive move-to-window-line 0)  goto-map)
(bind-key "b" (make-interactive move-to-window-line -1)  goto-map)
;;;_. Typical window operations but faster
(bind-key "M-0" 'delete-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-vertically)
(bind-key "M-3" 'split-window-horizontally)
;;;_. Windows configurations
(define-key global-map [(control x) (super left)] 'winner-undo)
(define-key global-map [(control x) (super right)] 'winner-redo)

;;;_ Help and Info
;;; ----------------------------------------------------------------------
(require 'help-mode+ nil t)
(require 'help+ nil t)
(require 'help-fns+ nil t)
;;; apropos seems to be more useful than apropos-command
(bind-key "C-h a" 'apropos)

;;;_ General Editing
;;;_. 'thingatpt' and `thingatpt+' libraries
(use-package thingatpt
  :defer t
  :config (progn
            (use-package thingatpt+
              :ensure t
              :config (progn
                        (tap-redefine-std-fns)
;;;_ , ffy-tap-number-change
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

;;;_ , ffy-tap-number-decrease
                        (defun ffy-tap-number-decrease (&optional num)
                          "Decreases number at the point by `num' or 1 if argument is not given"
                          (interactive "p")
                          (ffy-tap-number-change (- (or num 1))))

;;;_ , ffy-tap-number-increase
                        (defun ffy-tap-number-increase (&optional num)
                          "Increases number at the point by `num' or 1 if argument is not given"
                          (interactive "p")
                          (ffy-tap-number-change (or num 1)))))))

;;;_. Increment/Decrement number at the point
(eval-after-load "thingatpt"
  '(progn
     (bind-key "C--"  'ffy-tap-number-decrease)
     (bind-key "C-+"  'ffy-tap-number-increase)))

;;;_. Highlighting and colouring
(use-package idle-highlight-mode :ensure t)
(use-package rainbow-mode        :ensure t)
(use-package rainbow-delimiters  :ensure t)

;;;_ , highlight the "word" the cursor is on
(use-package highlight-symbol
  :ensure t
  :config  (progn
             (highlight-symbol-mode +1)
             (bind-key "<C-return>" 'highlight-symbol-at-point      ctl-z-map)
             (bind-key "<C-up>"     'highlight-symbol-prev          ctl-z-map)
             (bind-key "<C-down>"   'highlight-symbol-next          ctl-z-map)
             (bind-key "@"          'highlight-symbol-query-replace ctl-z-map)))

;;;_. Kill-rings
(use-package browse-kill-ring
  :ensure t
  :config  (progn
             (browse-kill-ring-default-keybindings) ; advises M-y
             (bind-key "C-x C-y" 'browse-kill-ring)))

(use-package kill-ring-search
  :ensure t
  :config  (progn
             (bind-key "C-M-y" 'kill-ring-search)))

;;;_. Undo settings
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))


;;;_. Enable useful disabled Narrow/Widen commands
(dolist (command '(narrow-to-region narrow-to-defun narrow-to-page widen set-goal-column))
  (put command 'disabled nil))

;;;_. Completion
;;;_ , Regular hippie-expand
;;; Naturally `hippie-expand-try-functions-list' would be made local
;;; variable and adjusted for a mode in the mode settings
(bind-key "M-/"  'hippie-expand)

;;;_ , Auto-Complete
(use-package auto-complete
  :disabled t
  :ensure t
  :init (progn
          (use-package popup :ensure t)
          (use-package fuzzy :ensure t)
          (use-package auto-complete-config :ensure t)
          (use-package pos-tip :ensure t)

          ;; add our own directory to the end of the list
          (add-to-list 'ac-dictionary-directories (concat *data-dir* "ac-dict") t)
          (setq ac-comphist-file (concat *data-dir* "ac-comphist.dat"))
          (ac-config-default)
          (global-auto-complete-mode t)
          ;(setq ac-auto-show-menu t)
          ;(setq ac-dwim t)
          ;(setq ac-use-menu-map t)
          ;(setq ac-quick-help-delay 1)
          ;(setq ac-quick-help-height 60)
          ;;(setq ac-disable-inline t)
          ;(setq ac-show-menu-immediately-on-auto-complete t)
          ;(setq ac-auto-start 2)
          ;(setq ac-candidate-menu-min 0)

          (set-default 'ac-sources
                       '(
                         ac-source-abbrev
                         ac-source-imenu
                         ac-source-dictionary
                         ac-source-words-in-buffer
                         ac-source-words-in-same-mode-buffers
                         ac-source-yasnippet
                         ))

          ;; FIX: fixing issue with ac-prefix-symbol with thingatpt+
          ;; If bounds-of-thing-at-point has been redefined (and we did so)
          ;; this function will return nil.
          (defun ac-prefix-symbol ()
            "Overriden default prefix definition function."
            (let ((symbol-start (car-safe (bounds-of-thing-at-point 'symbol))))
              (if (and (null symbol-start)
                       (fboundp 'tap-bounds-of-thing-nearest-point))
                  ;; try tap- function if available
                  (car-safe (tap-bounds-of-thing-nearest-point 'symbol))
                ;; else
                symbol-start)))))


;;;_  . Zapping
;;;_   , Zap-up-to char is a better alternative
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.
  \(fn arg char)" 'interactive)
;;;_   , zap-to-char-backwards
(defun zap-to-char-backwards (char)
    (interactive "cZap to char backwards: ")
    (zap-to-char -1 char))
;;;_   , zap-up-to-char-backwards
(defun zap-up-to-char-backwards (char)
    (interactive "cZap up to char backwards: ")
    (zap-up-to-char -1 char))
;;;_   , zapping key bindings
(bind-key "C-M-z"   'zap-to-char-backwards)
(bind-key "M-Z"     'zap-up-to-char)
(bind-key "C-M-S-z" 'zap-up-to-char-backwards)

;;;_  . Search
(bind-key "C-S-r"  'search-backward)
(bind-key "C-S-s"  'search-forward)

;;;_  . Navigation and Positioning

;;;_   , ffy-bol-or-back-to-indent
(defun ffy-bol-or-back-to-indent ()
  "In addition to having two different mappings for
 (move-beginning-of-line ARG) and (back-to-indentation) we
 will have a function that goes to BOL if we are on the
 indent position and to the indent if we are at the BOL"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))
;;;_   , redefine C-a to C-S-a and C-a to the ffy-bol-or-back-to-indent
(bind-key "C-S-a" (key-binding [(control ?a)]))
(bind-key "C-a"  'ffy-bol-or-back-to-indent)

;;;_  . use C-\ to leave one space between words
(define-key global-map [(control ?\\)] 'just-one-space)

;;;_  . Mark/Point machinery

;;; see
;;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/

;;; pushes mark into a ring without activating a region
(bind-key  "M-SPC"
  (make-interactive (lambda ()
                      (push-mark (point) t nil)
                      (message "Position %s pushed to the ring" (point)))))

;;;_  . there's default M-^ `delete-indentation' that is an alias to join-line
(bind-key "j" 'join-line ctl-z-map)
(bind-key "J" (lambda () "joins next line to this one"
                               (interactive)
                               (join-line 1)) ctl-z-map)
;;;_  . mark commands from `thing-cmds'
(use-package thing-cmds
  :ensure t
  :init (thgcmd-bind-keys))

;;;_  . miscellaneous
;;;_   , toggles line numbers in the buffer
(bind-key "C-S-l"  'linum-mode)
;;;_   , IMenu defaults
(set-default 'imenu-auto-rescan t)

;;;_ , Company
;;; Due to inconveniences of the `auto-complete' package use company
;;; instead
(use-package company
  :ensure t
  :diminish company-mode
  :config (progn
            (setq company-idle-delay 0.3
                  company-tooltip-limit 20
                  company-minimum-prefix-length 2
                  company-echo-delay 0))
  :init (global-company-mode 1))

;;;_ Miscellaneous
;;; ----------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(random t)

;;;_ URL uses data directory for its stuff
(setq url-configuration-directory (file-name-as-directory (concat *data-dir* "url")))

;;;_ Ack/Grep/RGrep
(use-package ack-and-a-half
  :ensure t
  :commands (ack-and-a-half ack-and-a-half-same ack-and-a-half-find-file ack-and-a-half-find-file-same)
  :init (progn
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same)
          (defalias 'ack-find-file 'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

(use-package grep
  :defer t
  :config
  (progn
    (setq wgrep-enable-key "e")
    (bind-key "e" 'wgrep-change-to-wgrep-mode  grep-mode-map)))

;;;_ Minibuffer and Smex
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


;;;_ Using smerge for merging files
;;; ----------------------------------------------------------------------
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;;;_ Bookmarking
;;; ----------------------------------------------------------------------
(use-package bm
  :ensure bm)

(use-package bookmark
  :defer t
  :config
  (progn
    (use-package bookmark+
      :ensure t)))

;;;_ Yasnippets
;;; ----------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (progn
    (use-package dropdown-list
      :ensure t)
    (add-to-list 'yas-snippet-dirs (concat *data-dir*  "snippets"))
    (yas-global-mode +1)))

;;;_ Ido configuraiton
;;; Some IDO settings that have been taken out from the customization file.
(use-package ido
  :config
  (progn
    (use-package ido-ubiquitous :ensure t)
    (ido-mode t)
    (ido-everywhere t)
    (ido-ubiquitous-mode t)

    ;; not every command should could be ido-ed
    ;; kill-ring-search has already set of minibuffer commands that don't
    ;; work well with ido-completing-read
    (setq ido-ubiquitous-command-exceptions '(kill-ring-search))
;;;_. ffy--change-ido-override
    (defun ffy--change-ido-override (behavior func-name)
      "Changes `ido-ubiquitous-function-overrides` variable for a function FUNC-NAME by setting its behavior to BEHAVIOR"
      (setq ido-ubiquitous-function-overrides
            (mapcar (lambda (override) (if  (equal (caddr override) ,func-name)
                                      (cons ,behavior (cdr override))
                                    override))
                    ido-ubiquitous-function-overrides)))
;;;_. enable-ido-for
    (defmacro enable-ido-for (func-name)
      "Enables IDO for a function using `ido-ubiquitous' mode"
      `(ffy--change-ido-override 'enable ,func-name))
;;;_. disable-ido-for
    (defmacro disable-ido-for (func-name)
      "Disables IDO for a function using `ido-ubiquitous' mode"
      `(ffy--change-ido-override 'disable ,func-name))))

;;;_ Version Control Systems
;;;_. Git
(use-package magit
  :ensure t
  :commands magit-status
  ;; Added global shortcut to run Magit
  :bind ("C-x g" . magit-status))

;;;_ Customizing Modes

;;;_. AllOut customizations
(use-package allout
  :diminish (allout-mode . "AO")
  :config  (progn
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
;(dolist (mode '(nxml-mode))
;  (add-to-list 'ac-modes mode))

;;;_. HTML and XHTML and other markup mode setup setup
;; (dolist (mode '(html-mode yaml-mode  textile-mode))
;;   (add-to-list 'ac-modes mode)


;;;_. Org Mode
(use-package org
  :defer t
  :bind (("C-&" . org-mark-ring-goto)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
         ;(bind-key "C-&" 'org-mark-ring-goto  mode-specific-map) ;; due to the conflict with Yasnippet
  :init (progn
          (setq org-completion-use-ido t
                ;; org-completion-use-iswitchb t     ; without it ido completion is
                ;;                                   ; not going to work for
                ;;                                   ; org-mode (see `org-read-property-value')
                org-hide-leading-stars t
                org-return-follows-link t
                org-modules '(org-docview
                              org-gnus
                              org-id
                              org-info
                              org-jsinfo
                              org-protocol
                              org-special-blocks
                              org-w3m
                              org-bookmark
                              org-elisp-symbol
                              org-panel)
                org-empty-line-terminates-plain-lists t)

          ;; make company completion work in Org-Mode
          (defun add-pcomplete-to-capf ()
            (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

          ;(add-hook 'org-mode-hook #'add-pcomplete-to-capf)
          (dolist (it '(turn-on-font-lock
                        yas-minor-mode-on
                        turn-on-auto-fill
                        turn-on-flyspell
                        hl-line-mode
                        add-pcomplete-to-capf
                        iimage-mode))
                  (add-hook 'org-mode-hook it))

          )
  :config (progn
            ;; Override not working function from org-mode
            (defun org-read-property-value (property)
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
                  val)))
            )

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


;(dolist (mode '(org-mode))
;  (add-to-list 'ac-modes mode))

;;
;;  Setup iimage working with Org-mode
;;
;; (add-hook 'org-mode-hook 'turn-on-iimage-mode)

;; (defun org-toggle-iimage-in-org ()
;;   "display images in your org file"
;;   (interactive)
;;   (if (face-underline-p 'org-link)
;;       (set-face-underline-p 'org-link nil)
;;     (set-face-underline-p 'org-link t))
;;   (iimage-mode))


)


;;;_. Orgtbl mode
;;; More about orgtbl:
;;; http://dynamic-thinking.blogspot.com/2009/11/orgtbl-mode.html
(use-package orgtbl
  :disabled t
  :commands orgtbl-mode
  :config (progn
            (defun orgtbl-to-gfm (table params)
      "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown.
Usage Example:
  <!--- BEGIN RECEIVE ORGTBL ${1:YOUR_TABLE_NAME} -->
  <!--- END RECEIVE ORGTBL $1 -->

  <!---
  #+ORGTBL: SEND $1 orgtbl-to-gfm
   | $0 |
  -->
For more details see https://gist.github.com/grafov/8244792 and https://gist.github.com/yryozo/5807243
"
      (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                   org-table-last-alignment ""))
             (params2
              (list
               :splice t
               :hline (concat alignment "|")
               :lstart "| " :lend " |" :sep " | ")))
        (orgtbl-to-generic table (org-combine-plists params2 params))))))

;;;_. Markdown
(use-package markdown-mode
  :config
  (progn
    (defun set-markdown-mode-outline-regexp ()
      "Add Markdown mode specifics.  Make outline-mode navigation work for underline headers as well"
      (make-local-variable 'outline-regexp)
      (setq outline-regexp "#+\\|^\\(.*\\)\n\\(===+\\|---+\\)$"))

    (add-hook 'markdown-mode-hook 'set-markdown-mode-outline-regexp)
    (add-hook 'markdown-mode-hook 'orgtbl-mode)))

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

;;;_ , All Lisps
;;;_  . Lists of all the lisp modes we deal with in Emacs 
;;;  modes that deal with EmacsLisp
(defconst *emacs-lisp-modes* '(emacs-lisp-mode lisp-mode ielm-mode))
;;;  list of all lisp modes
(defconst *lisp-modes* (cons 'clojure-mode *emacs-lisp-modes*))

;;;_  . Paredit settings
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config (progn
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

;;;_  . ffy-init-lispish-mode
(defun ffy-init-lispish-mode ()
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

;;;_  . Add setup to all lisp modes hooks
(dolist (mode *lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-lispish-mode)))

;;;_ , Emacs Lisps
;;;_  . Slime-like navigation in emacs
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode)
;;;_  . ffy-init-emacs-lisp-modes
(defun ffy-init-emacs-lisp-modes ()
  "Only emacs-lisp related things."
  (progn
    (make-local-variable 'hippie-expand-try-functions-list)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol 'to-the-end)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially 'to-the-end)
    (elisp-slime-nav-mode 1)))
;;;_  . Add setup to all emacs-lisp modes
(dolist (mode *emacs-lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-emacs-lisp-modes)))

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
;(dolist (mode '(inferior-emacs-lisp-mode))
;  (add-to-list 'ac-modes mode))

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
(use-package clojure-mode
  :ensure t
  :defer t
  :config (progn
            (use-package clojure-test-mode :ensure t :defer t)
            (use-package nrepl
              :ensure t
              :defer t
              :config (progn
                        (add-hook 'nrepl-mode-hook 'subword-mode)
                        (add-hook 'nrepl-mode-hook 'paredit-mode)
                        (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)))
            (use-package ac-nrepl
              :disabled t	
              :ensure t
              :config (progn
                        (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
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
            (add-hook 'clojure-mode-hook 'subword-mode)
            (add-hook 'clojure-mode-hook 'clojure-test-mode)

            ))

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
            (bind-key "<return>" 'reindent-then-newline-and-indent ruby-mode-map)
            (bind-key "#" 'ffy-insert-ruby-string-interpolation  ruby-mode-map)
            (bind-key "C-h r" 'yari  ruby-mode-map)
            (add-hook 'ruby-mode-hook 'subword-mode)
            (add-hook 'ruby-mode-hook 'ruby-electric-mode)
            (add-hook 'ruby-mode-hook 'ffy-run-programming-hook)
            (add-hook 'ruby-mode-hook 'flymake-ruby-load)
            (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
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
(use-package haml-mode
  :ensure t
  :commands haml-mode
  :mode ("\\.haml$" . haml-mode)
  :init (progn
          (use-package flymake-haml
            :ensure t
            :defer t
            :config (progn
                      (add-hook 'haml-mode-hook 'flymake-haml-load)))))
(use-package scss-mode
  :ensure t
  :defer t
  :commands scss-mode
  :mode ("\\.scss$" . scss-mode)
  :init (progn
          (use-package sass-mode
            :ensure t
            :defer t
            :commands sass-mode
            :mode ("\\.sass$" . sass-mode)
            :config (progn
                      (add-hook 'sass-mode-hook 'ffy-run-programming-hook)
                      (add-hook 'sass-mode-hook 'ffy-customize-sass-scss-mode)))

          (use-package flymake-sass
            :ensure t
            :defer t
            :config (progn
                      ;; my own customizations
                      (defun ffy-customize-sass-scss-mode ()
                        (interactive)
                        ;; first of all <ret> sets newline and indent as C-j
                        (local-set-key [return] 'newline-and-indent)
                        (local-set-key [(control return)] 'ffy-open-line-indented))

                      (add-hook 'scss-mode-hook 'flymake-sass-load)
                      (add-hook 'sass-mode-hook 'flymake-sass-load)))
          (add-hook 'scss-mode-hook 'ffy-run-programming-hook)
          (add-hook 'scss-mode-hook 'ffy-customize-sass-scss-mode)))

;;; custom line opening
(defun ffy-open-line-indented (n)
  "like `open-line' but keeps indentation"
  (interactive "*p")
  (let* ((loc (point-marker)))
    (newline-and-indent)
    (goto-char loc)))


;;; add Auto-Complete HAML SCSS and SASS modes
;(dolist (mode '(haml-mode sass-mode scss-mode))
;  (add-to-list 'ac-modes mode))

;;;_. Coffee-Script
(use-package coffee-mode
  :ensure t
  :defer t
  :commands coffee-mode
  :mode ("\\.coffee$" . coffee-mode)
  :config (progn
            (use-package flymake-coffee
              :ensure t
              :config (progn
                        (add-hook 'coffee-mode-hook 'flymake-coffee-load)))))

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
            ;(dolist (mode '(espresso-mode js-mode js2-minor-mode js2-mode))
            ;  (add-to-list 'ac-modes mode))
))
;;; Good package for remote debugging in the browser
;;; Read more at https://github.com/segv/jss
;;;
(use-package jss
  :ensure t
  :defer t)


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

;;;_. Octave Mode

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook (lambda ()
                              (auto-fill-mode 1)))

;;;_. Haskell Mode
(use-package haskell-mode
  :ensure t
  :init (progn
          (add-hook 'haskell-mode-hook 'ffy-run-programming-hook)
          (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

;;;_ Start Server
(server-start)

;;;_ Custom variables and faces
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

;;;_ Loading machine specific settings
(let ((system-specific-config (concat *system-specific* system-name ".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;;;_ How long did it take to load
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading Emacs...done (%.3fs)" elapsed))




;; Local Variables:
;; allout-layout: t
;; byte-compile-warnings: (not cl-functions)
;; End:
