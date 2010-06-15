;;  
;;  New Emacs Configuration
;;  Key Bindings
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; Turn on the menu bar for exploring new modes
(global-set-key [f1] 'menu-bar-mode)
(global-set-key [(control f1)] 'imenu-add-menubar-index)

;; We are trying to make keys working in both Windows and Mac OS X
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; File finding
(global-set-key [(f4)]      'recentf-open-most-recent-file)
;; TODO: check Mac OS X compatibility 
(global-set-key [(meta f4)] 'recentf-open-files)
(global-set-key [(control x) (meta ?f)] 'ido-find-file-other-window)
(global-set-key [(control x) (control meta ?f)] 'find-file-in-project)
(global-set-key [(control x) ?f] 'ido-choose-from-recentf)
(global-set-key [(control x) (control p)] 'find-file-at-point)
(global-set-key [(control c) ?y] 'bury-buffer)
(global-set-key [(control c) ?r] 'revert-buffer)
;; TODO: check Mac OS X compatibility 
(global-set-key [(meta ?`)] 'file-cache-minibuffer-complete)
(global-set-key [(control x) (control b)] 'ibuffer)


;; Windows
(global-set-key [(control ?z)] 'delete-window)
(global-set-key [(control x) (control ?z)] 'delete-other-windows)


(provide 'init-bindings)
