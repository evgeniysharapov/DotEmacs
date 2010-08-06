;;  
;;  New Emacs Configuration
;;  Startup
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defvar elpa-packages (list 'css-mode
                            'magit
			    'slime
                            'htmlize
                            'idle-highlight
                            'log4j-mode
                            'paredit
                            'smart-tab
                            'yasnippet-bundle
                            'dired-isearch
                            'findr
                            'smex
                            'nxml
                            'nxml-mode
                            'muse)
  "Libraries that should be installed by default.")



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
