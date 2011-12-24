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

(defun turn-on-flyspell-prog-mode ()
  (when (and (boundp 'ispell-program-name) 
             (executable-find ispell-program-name))
    (flyspell-prog-mode)))

(defface prog-mode-watchword-face
  '((((background light)) (:foreground "Red" :bold t))
    (((background dark)) (:foreground "Orange" :bold t)))
  "Highlight words like TODO, FIXME and so on"
  :group 'faces)

(defface prog-mode-bugs-face
  '((((background light)) (:background "Red" :foreground "Yellow"))
    (((background dark)) (:background "Red" :foreground "Yellow")))
  "Highlight Bug 1234 and alike"
  :group 'faces)

(defun prog-mode-faces-add ()
  (font-lock-add-keywords
   nil
   '(("\\s<\\s-*\\(FIX\\(ME\\)?\\|TODO\\|XXX\\):?\\(.+\\)\\>" 1 'prog-mode-watchword-face t)
     ("\\s<\\s-*\\([Bb][Uu][Gg]\\s-+[0-9]+\\)\\s-*:?"  1 'prog-mode-bugs-face t))))

(add-hook '*programming-hook* 'local-column-number-mode)
(add-hook '*programming-hook* 'local-comment-auto-fill)
(add-hook '*programming-hook* 'turn-on-hl-line-mode)
(add-hook '*programming-hook* 'turn-on-save-place-mode)
(add-hook '*programming-hook* 'pretty-greek)
(add-hook '*programming-hook* 'prog-mode-faces-add)
(add-hook '*programming-hook* 'turn-on-linum)
(add-hook '*programming-hook* 'turn-on-flyspell-prog-mode)

(defun run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))


;; --------------------------------------------------
;;     Setup Lisp based modes (including Elisp)
;; --------------------------------------------------
;; redefine paredit keys
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [(control shift ?d)] (lambda () (paredit-forward-delete +1)))
     ;; unset C-right/C-left as it is used to jump words
     (define-key paredit-mode-map [(control left)] nil)
     (define-key paredit-mode-map [(control right)] nil)))

;;
;; Things that are needed in evey lisp-like language
;;
(dolist (mode '(emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name mode) "-mode-hook"))
   `(lambda ()
     (progn
       (when (fboundp 'paredit-mode) 
         (paredit-mode +1))
       (turn-on-eldoc-mode)
       (run-programming-hook)
       (when (fboundp 'highlight-parentheses-mode)
         (highlight-parentheses-mode +1))
       (when (fboundp 'rainbow-delimiters-mode)
         (rainbow-delimiters-mode))))))

;;; Adding paredit-mode for an eval-expression in minibuffer. RET
;;; works as an exit minibuffer with evaluation. 
(defun minibuffer-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (when (fboundp 'paredit-mode)
        (paredit-mode +1)
        )))
(add-hook 'minibuffer-setup-hook 'minibuffer-enable-paredit-mode)

;; --------------------------------------------------
;;                       Slime
;; --------------------------------------------------
(when (require 'slime-autoloads nil 'noerror)
  (slime-setup)
  (eval-after-load "slime"
    '(progn

       (setq slime-complete-symbol*-fancy t
             slime-complete-symbol-function 'slime-fuzzy-complete-symbol
             slime-net-coding-system 'utf-8-unix)
       ;; (setq slime-use-autodoc-mode nil)


       (slime-setup '(slime-fancy slime-asdf slime-banner slime-repl))

       (add-hook 'lisp-mode-hook
                 (lambda ()
                   (slime-mode t)))

       (add-hook 'inferior-lisp-mode-hook
                 (lambda ()
                   (inferior-slime-mode t)))

       (add-hook 'slime-repl-mode-hook
                 (lambda ()
                   (when (fboundp 'paredit-mode)
                     (paredit-mode +1))
                   ;; set some keys to behave like we are in paredit
                   ;; prevent grabbing DEL button from
                   ;; http://www.emacswiki.org/emacs/ParEdit
                   (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)))

       (setq slime-lisp-implementations
             `((clozurecl ("d:/evgeniy/lisp/ccl-1.7/wx86cl.exe"))
                ,@slime-lisp-implementations)))))


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
  ;;

  ;; --------------------------------------------------
  ;;       Haskell Mode 
  ;; --------------------------------------------------

                                        ;(load "haskell-site-file")
                                        ;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                                        ;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
                                        ;(add-hook 'haskell-mode-hook 'run-programming-hook)

  ;; --------------------------------------------------
  ;;       Python Mode 
  ;; --------------------------------------------------
                                        ;(load "python-mode-things")
                                        ;(add-hook 'python-mode-hook 'sen/py-indentation)
                                        ;(add-hook 'python-mode-hook 'sen/py-autocomplete)
                                        ;(add-hook 'python-mode-hook 'sen/python-auto-fill-comments-only)
                                        ;(add-hook 'python-mode-hook
                                        ;'run-programming-hook)

(provide 'init-progmodes)
