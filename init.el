;;;
;;;  New Emacs Configuration
;;;  Startup
;;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;;

;;; debug if there's an error during loading
(let ((debug-on-error t))
(setq message-log-max 10000)
(defconst *emacs-start-time* (current-time))

;;; it is hard to do anything without common-lisp
(eval-when-compile
   (require 'cl))

;;; ---------------------------------------------------------------------------
;;; Load path configuration
;;; ---------------------------------------------------------------------------
(defconst *dotfiles-dir*
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Directory for dot files of Emacs configuration, i.e. path to .emacs.d directory")
(defconst *site-lisp*
  (file-name-as-directory (concat *dotfiles-dir* "site-lisp"))
  "Directory for Emacs Extensions files")
(defconst *autoload-file*
  (concat *dotfiles-dir* "loaddefs.el")
  "This is file containing all autoloads extracted from Emacs lisp files")

;;; adding dot files to the load path
(add-to-list 'load-path *dotfiles-dir*)

;;; add recursively all subdirectories of *site-lisp*
;;; using temporary recursive function so not to clutter function space
(cl-labels ((add-directory-to-path (dir)
                              (add-to-list 'load-path dir)
                              (dolist (entry (directory-files-and-attributes dir))
                                (if (and (cadr entry) ; t for directory
                                         (not (member (car entry) '("." "..")))) ; we don't want to deal with . and ..
                                    (let ((new-directory (expand-file-name (car entry) dir)))
                                      (add-to-list 'load-path new-directory)
                                      (add-directory-to-path new-directory))))))
  (add-directory-to-path *site-lisp*))

;;; Everyday functionality using REQUIRE
(mapc #'require '(uniquify saveplace))

;;; loading autoloads
(load *autoload-file* 'noerror)

;;; load my customization
(require 'custom-exts)
(require 'custom-utils)
(require 'custom-general)
(require 'custom-modes)
(require 'custom-bindings)

;;; start server
(server-start)

;;; Custom variables and faces
(setq custom-file (concat *dotfiles-dir* "custom.el"))
(load custom-file 'noerror)

;; loading customization settings specific for a system
(let ((system-specific-config (concat *dotfiles-dir* system-name ".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;;; How long did it take to load
(let ((elapsed (float-time (time-subtract (current-time)  *emacs-start-time*))))
  (message "Loading...done (%.3fs)" elapsed))
)
