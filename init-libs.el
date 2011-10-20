;;  
;;  Emacs Extension Management Configuration
;;  As the source for extensions we use ELPA and El-Get
;; 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;


(require 'package)
;; all ELPA packages are located here
(setq package-user-dir (concat *site-lisp* "/elpa"))

(setq package-archives '(;("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
;; ELPA packages that should be installed by default

(package-initialize)

(provide 'init-libs)
