;;  
;;  New Emacs Configuration
;;  Setup for a programming language modes 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; This variable contains functions that we need to run if we are programming 
(defvar *programming-hook*)
;; --------------------------------------------------
;; Stuff that is good to have
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
;; redefine paredit keys 
(eval-after-load "paredit"
  '(progn 
     (define-key paredit-mode-map [(return)] 'paredit-newline)
     (define-key paredit-mode-map [(control shift ?d)] (lambda () (paredit-forward-delete +1)))
     ;; unset C-right/C-left as it is used to jump words 
     (define-key paredit-mode-map [(control left)] nil)
     (define-key paredit-mode-map [(control right)] nil)))

(defface dimmed-paren
  '((((class color))
     :foreground (face-attribute 'font-lock-comment-face :foreground)))
  "Dim parens in Lisp-like languages")

(defun turn-on-paren-dim (mode)
  "Adds a new font-lock-kw for dimming parens in lisp based modes"
  (when window-system
    (font-lock-add-keywords
     (intern mode)
     '(("(\\|)" . 'dimmed-paren)))))
;;
;; Things that are needed in evey lisp-like language 
;;
(dolist (mode '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name mode) "-mode-hook"))
   `(lambda ()
     (progn
       (paredit-mode +1)
       (turn-on-eldoc-mode)
       (idle-highlight +1)
       (run-programming-hook)
       (turn-on-paren-dim ,(concat (symbol-name mode) "-mode"))
       (if (fboundp 'highlight-parentheses-mode)
           (highlight-parentheses-mode +1))))))

;; --------------------------------------------------
;;                       Slime 
;; --------------------------------------------------
(eval-after-load "slime"
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (progn
                (paredit-mode +1)
                (setq slime-net-coding-system 'utf-8-unix)))))

;; --------------------------------------------------
;;                       Clojure
;; --------------------------------------------------
;; (eval-after-load "slime"
;;    '(progn
;;       (require 'swank-clojure-extra)
;;       (add-to-list 'slime-lisp-implementations
;;                    `(clojure ,(swank-clojure-cmd)
;                             :init swank-clojure-init)
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
