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

;;; ------------------------------------------------------------
;;;  File Operations 
;;; ------------------------------------------------------------
(global-set-key [(f4)]      'recentf-open-most-recent-file)
(global-set-key [(meta f4)] 'recentf-open-files)
(global-set-key [(control x) (meta ?f)] 'ido-find-file-other-window)
(global-set-key [(control x) (control meta ?f)] 'find-file-in-project)
(global-set-key [(control x) ?f] 'ido-choose-from-recentf)
(global-set-key [(control x) (control p)] 'find-file-at-point)
(global-set-key [(meta ?`)] 'file-cache-minibuffer-complete)

;;; ------------------------------------------------------------
;;;  Buffer Operations 
;;; ------------------------------------------------------------
(global-set-key [(control c) ?y] 'bury-buffer)
(global-set-key [(control c) ?r] 'revert-buffer)
(global-set-key [(control x) (control b)] 'ibuffer)
(define-key global-map [f11] 'previous-buffer)
(define-key global-map [f12] 'next-buffer)

;;;; Borrowed this idea from http://www.jurta.org/en/emacs/dotemacs
;;; C-z my-map
;; Make the prefix key `C-z' for my personal keymap.
;; On qwerty-keyboards `C-z' is one of the most accessible keys
;; like `C-x' and `C-c', but the prefix key `C-c' is reserved
;; for mode-specific commands (both user-defined and standard Emacs extensions).
;; The standard binding of `C-z' (`suspend-emacs' or `iconify-or-deiconify-frame')
;; is reassigned here to double key sequence `C-z C-z'.
(defvar my-map
  (let ((map (make-sparse-keymap))
        (c-z (global-key-binding [(control ?z)])))
    (global-unset-key [(control ?z)])
    (define-key global-map [(control ?z)] map)
    (define-key map [(control ?z)] c-z)
    map))

;;; ------------------------------------------------------------------ 
;;;  Windows Operations
;;; ------------------------------------------------------------------
;; Windmove
(global-set-key [(shift left)] 'windmove-left)
(global-set-key [(shift right)] 'windmove-right)
(global-set-key [(shift up)] 'windmove-up)
(global-set-key [(shift down)] 'windmove-down)
;;; Moving in a window
(define-key goto-map [(?t)] (make-interactive move-to-window-line 0))
(define-key goto-map [(?b)] (make-interactive move-to-window-line -1))
;; switch windows
(global-set-key [(meta ?o)] 'other-window)
;; Typical window operations but faster
(global-set-key [(meta ?0)] 'delete-window)
(global-set-key [(meta ?1)] 'delete-other-windows)
(global-set-key [(meta ?2)] 'split-window-vertically)
(global-set-key [(meta ?3)] 'split-window-horizontally)

;;; ------------------------------------------------------------
;;;  Editing/Operations In Buffer
;;; ------------------------------------------------------------
;;; toggles line  numbers in the buffer
(global-set-key [(control x) (control shift ?l)] 'linum-mode)
(global-set-key [(control shift ?r)] 'search-backward)
(global-set-key [(control shift ?s)] 'search-forward)

;;; ------------------------------------------------------------
;;; Kebindings for Extensions / Non standard Emacs Functinoality
;;; ------------------------------------------------------------

;; Added global shortcut to run Magit
(when (fboundp 'magit-status)
  (define-key global-map [(control x) ?g] 'magit-status))

;; we use kill-ring-search through ELPA, hence check if it is
;; available first
(when (fboundp 'kill-ring-search)
  (global-set-key [(control meta ?y)] 'kill-ring-search))

;; browse kill ring is nice too and also might be unavailable
(when (fboundp 'browse-kill-ring)
  (global-set-key [(control x) (control ?y)] 'browse-kill-ring))

(provide 'init-bindings)
