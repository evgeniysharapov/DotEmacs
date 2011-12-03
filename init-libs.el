;;  
;;  Emacs Extension Management Configuration
;;  As the source for extensions we use ELPA and El-Get
;; 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun require-package (package &optional min-version)
  "Installs package of desired version using ELPA or el-get"
  ;; TODO: make it work with minimum and maximum version
  ;; use version-list-= and version-list-> from subr.el
  (unless (package-installed-p package min-version)
    (package-install package)))


(when (require 'package nil 'noerror)
   ;; all ELPA packages are located here
   (setq package-user-dir (concat *dotfiles-dir* "elpa"))
   ;; ELPA extensions repos 
   (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
   (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  ;; ELPA packages that I would like to have installed
  (require-package 'magit)

  (require-package 'idle-highlight-mode)
  (require-package 'highlight-symbol)
  (require-package 'rainbow-mode)
  (require-package 'rainbow-delimiters)
  
  (require-package 'paredit)

  (require-package 'ido-yes-or-no)
  (require-package 'ido-ubiquitous)
  
  (require-package 'smex)
  (require-package 'yasnippet-bundle)
  (require-package 'undo-tree)
  
  (require-package 'org)
)

(provide 'init-libs)
