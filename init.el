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

;; add recursively all subdirectories of *site-lisp* without
;; cluttering function space
(eval-when-compile
   (require 'cl))
(flet ((add-directory-to-path (dir)
                              (add-to-list 'load-path dir)
                              (dolist (entry (directory-files-and-attributes dir))
                                (if (and (cadr entry) ; t for directory
                                         (not (member (car entry) '("." "..")))) ; we don't want to deal with . and ..
                                    (let ((new-directory (expand-file-name (car entry) dir)))
                                      (add-to-list 'load-path new-directory)
                                      (add-directory-to-path new-directory))))))
  (add-directory-to-path *site-lisp*))

;; Everyday functionality using REQUIRE
(mapc #'require '(uniquify saveplace))

;; loading autoloads
(load *autoload-file* 'noerror)

;; load my customization 
(require 'init-exts)
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
