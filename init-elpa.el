;;  
;;  New Emacs Configuration
;;  Startup
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;
(require 'package)
(setq package-user-dir (concat site-lisp "/elpa"))
;(package-initialize)

(defvar elpa-packages (list 'idle-highlight
                            'paredit
                            'smart-tab
                            'yasnippet-bundle
                            'smex
			    'org
                            'muse)
  "Libraries that should be installed by default.")

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

(defun elpa-install ()
  "Install all packages that aren't installed."
  (interactive)
  (dolist (pkg elpa-packages)
    (unless (or (member pkg package-activated-list)
                (functionp pkg))
      (message "Installing %s" (symbol-name pkg))
      (package-install pkg))))

(defun online? ()
  "See if we're online. Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (online?)
  (unless package-archive-contents (package-refresh-contents))
  (elpa-install))


(provide 'init-elpa)
