;;  
;;  Emacs Extension Management Configuration
;;  As the source for extensions we use ELPA and El-Get
;;  Also we use packages in site-lisp directory
;;
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun ffy-find-package-autoloads-file (package)
  "Finds autoload file for the ELPA package and returns the path to the file, with a  check for file existence if performed, or nil.
One of the examples of using the 

 (ffy-find-package-autoloads-file 'haskell-mode)"
  ;; get version of the package version list to string 
  (let* ((pkg-str (if (symbolp package) (symbol-name package) package))
         (pkg-ver-str (mapconcat 'number-to-string 
                                 (elt (cdr  (assq package package-alist)) 0) "."))
         (pkg-al-file (concat (package--dir pkg-str pkg-ver-str) "/" pkg-str "-autoloads.el")))
    ;; check file existence 
    (when (file-exists-p pkg-al-file)
      pkg-al-file)))

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
  ;; VCS
  (require-package 'magit)
  ;; look options
  (require-package 'idle-highlight-mode)
  (require-package 'highlight-symbol)
  (require-package 'rainbow-mode)
  (require-package 'rainbow-delimiters)
  ;; lispish modes
  (require-package 'paredit)
  (require-package 'elisp-slime-nav)
  (require-package 'clojure-mode)
  ;; extend functionality
  (require-package 'ido-yes-or-no)
  (require-package 'ido-ubiquitous)
  (require-package 'browse-kill-ring)
  (require-package 'kill-ring-search)
  (require-package 'smex)
  (require-package 'yasnippet-bundle)
  (require-package 'undo-tree)
  (require-package 'minimap)
  ;; miscelaneous useful modes
  (require-package 'markdown-mode)

  ;; ruby and rails setup
  (require-package 'rinari)
  (require-package 'rspec-mode)
  (require-package 'ruby-block)
  (require-package 'ruby-compilation)
  (require-package 'ruby-electric)
  (require-package 'ruby-end)
  (require-package 'ruby-mode)
  (require-package 'ruby-test-mode)
  (require-package 'ruby-tools)
  
  (require-package 'org))

(defun extract-autoloads ()
  "Extract autoloads recursively from *SITE-LISP* and puts it into *AUTOLOAD-FILE*"
  (interactive "f")
  (let* ((generated-autoload-file *autoload-file*)
         (buffer-file-coding-system 'no-conversion)
         ;; avoid generating autoloads for slime - results in error 
         ;; "Local variables entry is missing the suffix"
         (dir-list (loop for d in (directory-files *site-lisp* 'full "[^\(^\\.+$\|^slime\)]")
                         if (file-directory-p d)
                         collect d)))
    (apply 'update-directory-autoloads dir-list)))

(add-hook 'kill-emacs-hook 'extract-autoloads)

(provide 'init-exts)
