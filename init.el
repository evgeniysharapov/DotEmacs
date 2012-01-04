;;  
;;  New Emacs Configuration
;;  Startup
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; debug if there's an error dusinr loading
(let ((debug-on-error t))

;; We set up load paths first 

(defvar *dotfiles-dir* (file-name-directory (or (buffer-file-name) load-file-name)) "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")
(add-to-list 'load-path *dotfiles-dir*)
(defvar *site-lisp* (file-name-as-directory (concat *dotfiles-dir* "site-lisp")) "Directory for Emacs Extensions files")
(defvar *autoload-file* (concat *dotfiles-dir* "loaddefs.el") "This is file containing all autoloads extracted from Emacs lisp files")

;; add recursively all subdirectories of *site-lisp* 
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let ((default-directory *site-lisp*))
      (normal-top-level-add-subdirs-to-load-path)))

;; Everyday functionality using REQUIRE
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; We will use ELPA now 
(require 'init-libs)

(defun extract-autoloads ()
  "Extract autoloads recursively from *SITE-LISP* and puts it into *AUTOLOAD-FILE*"
  (interactive "f")
  (let* ((generated-autoload-file *autoload-file*)
         (bcc-blacklist (cons generated-autoload-file bcc-blacklist))
         (buffer-file-coding-system 'no-conversion)
         ;; avoid generating autoloads for slime - results in error 
         ;; "Local variables entry is missing the suffix"
         (dir-list (loop for d in (directory-files *site-lisp* 'full "[^\(^\\.+$\|^slime\)]")
                         if (file-directory-p d)
                         collect d)))
    (apply 'update-directory-autoloads dir-list)))

(add-hook 'kill-emacs-hook 'extract-autoloads)

(load *autoload-file* 'noerror)

;; load my customization 
(require 'init-defuns)
(require 'init-system)
(require 'init-general)
(require 'init-progmodes)
(require 'init-xml)
(require 'init-org)
(require 'init-bindings)

;; start server 
(server-start)

;; Custom variables and faces 
(setq custom-file   (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

;; loading customization settings specific for a system 
(setq system-specific-config (concat *dotfiles-dir* system-name ".el"))
(if (file-exists-p system-specific-config) (load system-specific-config))
)
