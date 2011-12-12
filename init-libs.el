;;  
;;  Emacs Extension Management Configuration
;;  As the source for extensions we use ELPA and El-Get
;; 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun ffy-find-package-autoloads-file (package)
  "Finds autoload file for the ELPA package and returns the path to the file, with a  check for file existence if performed, or nil."
  ;; get version of the package version list to string 
  (let* ((pkg-str (if (symbolp package) (symbol-name package) package))
         (pkg-ver-str (mapconcat 'number-to-string 
                                 (elt (cdr  (assq package package-alist)) 0) "."))
         (pkg-al-file (concat (package--dir pkg-str pkg-ver-str) "/" pkg-str "-autoloads.el")))
    ;; check file existence 
    (when (file-exists-p pkg-al-file)
      pkg-al-file)))

;;; One of the examples of using the 
;;; (ffy-find-package-autoloads-file 'haskell-mode)

(defun require-package (package &optional min-version)
  "Installs package of desired version using ELPA"
  ;; TODO: make it work with minimum and maximum version
  ;; use version-list-= and version-list-> from subr.el
  (unless (package-installed-p package min-version)
    (package-install package))
  (let ((pkg-autoload (ffy-find-package-autoloads-file package)))
    (when pkg-autoload
      (load pkg-autoload t))))


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

  (require-package 'browse-kill-ring)
  (require-package 'kill-ring-search)
  
  (require-package 'smex)
  (require-package 'yasnippet-bundle)
  (require-package 'undo-tree)
  
  (require-package 'org)

  (require-package 'haskell-mode)
)

(provide 'init-libs)
