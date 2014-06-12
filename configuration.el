
(defconst *emacs-start-time* (current-time))

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

(add-to-list 'load-path *dotfiles-dir*)

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
(add-directory-to-path *site-lisp*)
;;; erase the function
(fmakunbound #'add-directory-to-path)

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

(mapc #'require '(uniquify saveplace))

(require 'cl-lib)

(unless (require 'use-package nil 'noerror)
   (let ((default-directory (file-name-directory load-file-name)))
         (shell-command "git submodule update --init"))
   (message "Updated use-package libraries"))
(mapc #'require '(use-package bind-key))

(defmacro keymap-on-key (name keys)
  "This is a macro that declares a variable, key prefix and assigns a key to it.
NAME is symbol of the new keymap and KEYS is a string that represents keys as for macro `kbd'"
  `(progn (defvar ,name)
          (define-prefix-command (quote ,name))
          (bind-key ,keys (quote ,name))))

(defvar ctl-z-map)
(define-prefix-command 'ctl-z-map)
(let ((c-z (global-key-binding [(control ?z)])))
  (global-unset-key [(control ?z)])
  (bind-key "C-z" 'ctl-z-map)
  (bind-key "C-z C-z" c-z))

(keymap-on-key ctl-x-f-map "C-x f")
