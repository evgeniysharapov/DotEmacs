;;  
;;  New Emacs Configuration
;;  Setup for a programming language modes 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; This variable contains functions that we need to run if we are programming 
(defvar *programming-hook*)

;; --------------------------------------------------
;;  Autocomplete 
;; --------------------------------------------------
(require 'auto-complete)
(global-auto-complete-mode t)
(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))
;; --------------------------------------------------
;;  Other programming modes nifties 
;; --------------------------------------------------
(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun turn-on-linum ()
  (linum-mode +1))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook '*programming-hook* 'local-column-number-mode)
(add-hook '*programming-hook* 'local-comment-auto-fill)
(add-hook '*programming-hook* 'turn-on-hl-line-mode)
(add-hook '*programming-hook* 'turn-on-save-place-mode)
(add-hook '*programming-hook* 'pretty-greek)
(add-hook '*programming-hook* 'add-watchwords)
(add-hook '*programming-hook* 'turn-on-linum)

(defun run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))

;; --------------------------------------------------
;;     Setup Lisp based modes (including Elisp)
;; --------------------------------------------------
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

(defun turn-on-paredit (keymap) 
  (paredit-mode +1) 
  (define-key keymap (kbd "RET") 'paredit-newline))

(add-hook 'emacs-lisp-mode-hook (lambda () (turn-on-paredit emacs-lisp-mode-map)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)
(add-hook 'lisp-mode-hook 'pretty-greek)

(dolist (x '(scheme emacs-lisp lisp clojure))
  ;(when window-system
  ;  (font-lock-add-keywords
  ;   (intern (concat (symbol-name x) "-mode"))
  ;   '(("(\\|)" . 'esk-paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook"))
   (lambda ()
     (paredit-mode +1)
     (idle-highlight +1)
     (run-programming-hook))))

;; ;; these paredit keys are confusing
;; (eval-after-load 'paredit
;;   '(progn
;;      ;; it's a habit to use C-Arrows to move over words 
;;      (define-key paredit-mode-map  [(control right)] nil)
     
;;      (define-key paredit-mode-map  [(kbd "<M-down>")] nil)
;;      (define-key paredit-mode-map "\M-r" nil)))

;; ;; --------------------------------------------------
;; ;;  Clojure
;; ;; --------------------------------------------------
;; (eval-after-load "slime"
;;   '(progn
;;      (require 'swank-clojure-extra)
;;      (add-to-list 'slime-lisp-implementations
;;                   `(clojure ,(swank-clojure-cmd)
;;                             :init swank-clojure-init)
;;                   t)
;;      (add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
;;      (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
;;      (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)
;;      (setq slime-highlight-compiler-notes nil)))

;; ;; 




;; --------------------------------------------------
;;       Haskell Mode 
;; --------------------------------------------------

(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'run-programming-hook)

;; --------------------------------------------------
;;       Python Mode 
;; --------------------------------------------------
;(load "python-mode-things")
;(add-hook 'python-mode-hook 'sen/py-indentation)
;(add-hook 'python-mode-hook 'sen/py-autocomplete)
;(add-hook 'python-mode-hook 'sen/python-auto-fill-comments-only)
;(add-hook 'python-mode-hook 'run-programming-hook)

(provide 'init-progmodes)
