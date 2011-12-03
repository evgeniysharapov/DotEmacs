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

;; Set up local path list (from where we will regenerate autoloads 
(setq local-load-path (list *site-lisp*))

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

;; Autoloads and custom files 
(setq autoload-file (concat *dotfiles-dir* "loaddefs.el"))
(when  (not (file-exists-p autoload-file))
  (let (emacs-lisp-mode-hook
        (generated-autoload-file autoload-file))
    (dolist (path local-load-path)
              (message "Updating autoloads from %s" path)
              (update-directory-autoloads path))))
(load autoload-file)

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