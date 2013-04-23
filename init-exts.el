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

(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;;------------------------------------------------------------------------------
;; Add support to package.el for pre-filtering available packages
;;------------------------------------------------------------------------------
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            (funcall package-filter-function
                     (car package)
                     (package-desc-vers (cdr package))
                     archive))
    ad-do-it))

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

  ;; Don't take Melpa versions of certain packages
  (setq package-filter-function
	(lambda (package version archive)
	  (or (not (string-equal archive "melpa"))
	      (not (memq package
			 '(
			   ruby-compilation
			   slime
			   color-theme-sanityinc-solarized
			   color-theme-sanityinc-tomorrow
			   elisp-slime-nav
			   findr))))))

  ;; ELPA extensions repos 
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
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
  (require-package 'diminish)
  ;; lispish modes
  (require-package 'paredit)
  (require-package 'elisp-slime-nav)
  ;; clojure
  (require-package 'clojure-mode)
  (require-package 'clojure-test-mode)
  (require-package 'nrepl)
  (require-package 'ac-nrepl)
  ;; extend functionality
  (require-package 'ido-yes-or-no)
  (require-package 'ido-ubiquitous)
  (require-package 'browse-kill-ring)
  (require-package 'kill-ring-search)
  (require-package 'smex)
  (require-package 'yasnippet)
  (require-package 'undo-tree)
  (require-package 'minimap)
  (require-package 'bm)
  (require-package 'bookmark+)
  (require-package 'ack-and-a-half)
  (require-package 'find-file-in-project)

  ;; miscelaneous useful modes
  (require-package 'markdown-mode)

  ;; ruby and rails setup
  (require-package 'rinari)
  (require-package 'rspec-mode)
  (require-package 'ruby-compilation)
  (require-package 'ruby-electric)
  (require-package 'rvm)
  (require-package 'yari)
  (require-package 'flymake-ruby)
  (require-package 'flymake-haml)
  (require-package 'flymake-sass)
  (require-package 'flymake-coffee)
  ;; scala
  (require-package 'scala-mode)
)

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
