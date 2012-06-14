;;  
;;  New Emacs Configuration
;;  Key Bindings
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; Turn on the menu bar for exploring new modes
(define-key global-map [f1] 'menu-bar-mode)
(define-key global-map [(control f1)] 'imenu-add-menubar-index)

;; We are trying to make keys working in both Windows and Mac OS X
;; To be able to M-x without meta
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)

;;; apropos seems to be more useful than apropos-command
(define-key global-map [(control h) ?a] 'apropos)

;;; ------------------------------------------------------------
;;;  File Operations 
;;; ------------------------------------------------------------
(define-key global-map [(control x) (shift ?f)]      'recentf-open-most-recent-file)
(define-key global-map [(control x) (meta ?f)] 'ido-find-file-other-window)
(define-key global-map [(control x) (control meta ?f)] 'find-file-in-project)
(define-key global-map [(control x) ?f] 'ido-choose-from-recentf)
(define-key global-map [(control x) (control p)] 'find-file-at-point)

;;; ------------------------------------------------------------
;;;  Buffer Operations 
;;; ------------------------------------------------------------
(define-key global-map [(control x) (control b)] 'ibuffer)
(define-key global-map [f11] 'previous-buffer)
(define-key global-map [f12] 'next-buffer)

;;;; Borrowed this idea from http://www.jurta.org/en/emacs/dotemacs
;;; C-z ctrl-z-map
;; Make the prefix key `C-z' for my personal keymap.
;; On qwerty-keyboards `C-z' is one of the most accessible keys
;; like `C-x' and `C-c', but the prefix key `C-c' is reserved
;; for mode-specific commands (both user-defined and standard Emacs extensions).
;; The standard binding of `C-z' (`suspend-emacs' or `iconify-or-deiconify-frame')
;; is reassigned here to double key sequence `C-z C-z'.
(defvar ctrl-z-map
  (let ((map (make-sparse-keymap))
        (c-z (global-key-binding [(control ?z)])))
    (global-unset-key [(control ?z)])
    (define-key global-map [(control ?z)] map)
    (define-key map [(control ?z)] c-z)
    map))

;;; Buffer operations in C-z map
(define-key ctrl-z-map [?b ?y] 'bury-buffer)
(define-key ctrl-z-map [?b ?r] 'revert-buffer)

;;; ------------------------------------------------------------
;;;  Windows Operations
;;; ------------------------------------------------------------
;; Windmove
(windmove-default-keybindings) ;; Shift+direction
;;; Moving in a window
(define-key goto-map [(?t)] (make-interactive move-to-window-line 0))
(define-key goto-map [(?b)] (make-interactive move-to-window-line -1))
;; switch windows
(define-key global-map [(meta ?o)] 'other-window)
;; Typical window operations but faster
(define-key global-map [(meta ?0)] 'delete-window)
(define-key global-map [(meta ?1)] 'delete-other-windows)
(define-key global-map [(meta ?2)] 'split-window-vertically)
(define-key global-map [(meta ?3)] 'split-window-horizontally)

(define-key ctrl-z-map [(u)] 'winner-undo)
(define-key ctrl-z-map [(r)] 'winner-redo)

;;; ------------------------------------------------------------
;;;  Editing/Operations In Buffer
;;; ------------------------------------------------------------
;;; toggles line  numbers in the buffer
(define-key global-map [(control x) (control shift ?l)] 'linum-mode)
(define-key global-map [(control shift ?r)] 'search-backward)
(define-key global-map [(control shift ?s)] 'search-forward)

;;; ------------------------------------------------------------
;;; Kebindings for Extensions / Non standard Emacs Functinoality
;;; ------------------------------------------------------------

;; Added global shortcut to run Magit
(when (fboundp 'magit-status)
  (define-key global-map [(control x) ?g] 'magit-status))

;; we use kill-ring-search through ELPA, hence check if it is
;; available first
(when (fboundp 'kill-ring-search)
  (define-key global-map [(control meta ?y)] 'kill-ring-search))

;; browse kill ring is nice too and also might be unavailable
(when (fboundp 'browse-kill-ring)
  (browse-kill-ring-default-keybindings) ; advise M-y
  (define-key global-map [(control x) (control ?y)] 'browse-kill-ring))

;; Smex is used in minibuffer M-x
(when (fboundp 'smex-initialize)
  (define-key global-map [(meta ?x)] 'smex)
  (define-key global-map [(meta shift ?x)] 'smex-major-mode-commands))

;; highlight-symbol mode is available
(when (fboundp 'highlight-symbol-at-point)
  (define-key ctrl-z-map [(l)] 'highlight-symbol-at-point)
  (define-key ctrl-z-map [(j)] 'highlight-symbol-prev)
  (define-key ctrl-z-map [(k)] 'highlight-symbol-next)
  (define-key ctrl-z-map [(@)] 'highlight-symbol-query-replace))

(provide 'init-bindings)
