;;  
;;  New Emacs Configuration
;;  Startup
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(setq debug-on-error t)

;; We set up load paths first 

(defvar *dotfiles-dir* (file-name-directory (or (buffer-file-name) load-file-name)) "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")
(add-to-list 'load-path *dotfiles-dir*)
(defvar *site-lisp* (concat *dotfiles-dir* "site-lisp") "Directory for Emacs Extensions files")

;; Set up local path list (from where we will regenerate autoloads 
(setq local-load-path (list *site-lisp*))

(defun add-paths (this-directory path-list &optional with-subdirs recursive) 
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists. Add all its subdirectories not starting with a '.' if the optional argument WITH-SUBDIRS is not nil. Do it recursively if the third argument is not nil."
  (when this-directory 
    (when (file-directory-p this-directory) 
      (let* ((this-directory (expand-file-name this-directory)) 
             (files (directory-files this-directory t "^[^\\.]")))
        ;; completely canonicalize the directory name (*may not* begin with `~')
        (while (not (string= this-directory (expand-file-name this-directory))) 
          (setq this-directory (expand-file-name this-directory))) 
        
	(message "Adding `%s' to load-path..." this-directory) 
        (add-to-list path-list this-directory) 
        (if with-subdirs 
            (progn 
              (while files 
                (setq dir-or-file (car files)) 
                (when (file-directory-p dir-or-file) 
                  (if recursive 
		     (add-paths dir-or-file path-list 'with-subdirs)
                     (add-paths dir-or-file path-list))) 
                (setq files (cdr files)))))))))

;; add recursively all subdirectories of *site-lisp* 
(add-paths *site-lisp* 'local-load-path 'with-subdirs 'recursive)

;; add it to the load path 
(dolist (path local-load-path) (add-to-list 'load-path path))

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
