;;;
;;; Emacs Configuration
;;;
;;;
;;; Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;;

;;; borrowed from starter-kit and modified. this is neat and it effectively 
;;; loads Emacs with all the defaults, so that Org-mode will be available 
;;; on the load-path.
(add-hook 'after-init-hook
 `(lambda ()
  ;; toggling on debug-on-error will start debugging in case of the error.
  ;; very useful while making changes to the configuration
  (toggle-debug-on-error)
  (require 'org)
  (require 'ob-tangle)
  ;; To make ob-tangle happy even if I have crazy UTF characters
  ;; in my config org-mode file
  ;; for more see https://lists.gnu.org/archive/html/emacs-orgmode/2010-11/msg00951.html
  (add-hook 'org-babel-pre-tangle-hook
          (lambda ()
	    (setq default-buffer-file-coding-system 'utf-8)))
  ;; load up Emacs configuraion from the Org file next to init.el
  ;; filename is evaled while macros is expanded, otherwise
  ;; buffer-file-name would be nil
  ;; we check whether we need to babel load org file or
  ;; load converted emacs-lisp file
  (let* ((dir ,(file-name-directory (or load-file-name (buffer-file-name))))
         (org-file (expand-file-name "configuration.org" dir))
         (el-file (expand-file-name "configuration.el" dir)))
    (if (not (file-newer-than-file-p el-file org-file))
        (org-babel-load-file org-file)
      (load-file el-file)))
  ;; if we loaded it up, use normal error throwing
  (toggle-debug-on-error)
  (desktop-read)))
