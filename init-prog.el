;;
;;  New Emacs Configuration
;;  Setup for a programming language modes 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; --------------------------------------------------
;; General Programming Mode
;; --------------------------------------------------
(defvar *programming-hook* nil
  "This variable contains functions that we need to run if we are programming ")

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-whitespace ()
  (whitespace-mode t))

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
  "Highlights certain words, like BUG or XXX or FIXME in the code"
  (font-lock-add-keywords
   nil
   '(("\\s<\\s-*\\(FIX\\(ME\\)?\\|TODO\\|XXX\\):?\\(.+\\)\\>" 1 'prog-mode-watchword-face t)
     ("\\s<\\s-*\\([Bb][Uu][Gg]\\s-+[0-9]+\\)\\s-*:?"  1 'prog-mode-bugs-face t))))

(defun turn-on-flymake ()
  (flymake-mode))

(dolist   (it '(local-column-number-mode
                local-comment-auto-fill
                turn-on-hl-line-mode
                pretty-greek
                prog-mode-faces-add
                turn-on-flyspell-prog-mode
                turn-on-flymake))
  (if (fboundp it)
      (add-hook '*programming-hook* it)))

(defun ffy-run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))

;;; ------------------------------------------------------------
;;;                        Auto-Complete
;;; ------------------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *dotfiles-dir* "ac-dict"))
(ac-config-default)
(setq ac-dwim t
      ac-auto-start t)

;;;;;;;;;;;;;;; Specific Programming Languages Environment Follow ;;;;;;;;;

;; --------------------------------------------------
;;    Lisp-like Programming Lanuages
;; --------------------------------------------------
(defconst *emacs-lisp-modes* '(emacs-lisp-mode lisp-mode))
(defconst *lisp-modes* (cons 'clojure-mode *emacs-lisp-modes*))

;;; Paredit settings
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [(control shift ?d)] (lambda () (paredit-forward-delete +1)))))
;;; Slime Settings
(eval-after-load "slime"
  '(progn
     (message "Check if slime has been loaded !")

     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-net-coding-system 'utf-8-unix)

     (slime-setup '(slime-fuzzy slime-banner slime-repl))

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
     ))

(defun ffy-init-lisp-minibuffer-enable-paredit-mode ()
  "Enable function `paredit-mode' during `eval-expression'. Adding `paredit-mode' for an `eval-expression' in minibuffer. RET  works as an exit minibuffer with evaluation."
  (if (eq this-command 'eval-expression)
      (when (fboundp 'paredit-mode)
        (paredit-mode +1))))

(add-hook 'minibuffer-setup-hook 'ffy-init-lisp-minibuffer-enable-paredit-mode)

(defun ffy-init-lisp-setup ()
  "This is the setup that would any lisp based mode benefit from"
  (progn
       (when (fboundp 'paredit-mode) 
         (paredit-mode +1))
       (turn-on-eldoc-mode)
       (ffy-run-programming-hook)
       (when (fboundp 'highlight-parentheses-mode)
         (highlight-parentheses-mode +1))
       (when (fboundp 'rainbow-delimiters-mode)
         (rainbow-delimiters-mode))
       (define-key lisp-mode-shared-map [(meta return)] 'reindent-then-newline-and-indent)))

(defun ffy-init-lisp-emacs-setup ()
  "Only emacs-lisp related things."
  (progn
    (make-local-variable 'hippie-expand-try-functions-list)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol 'to-the-end)
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially 'to-the-end)
    (elisp-slime-nav-mode 1)
    (add-to-list 'ac-sources 'ac-source-emacs-lisp-features)))


(dolist (mode *lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-lisp-setup)))

(dolist (mode *emacs-lisp-modes*)
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook 'ffy-init-lisp-emacs-setup)))
;;;
;;; Clojure Specifics
;;;
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'clojure-test-mode)

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

;;; -----------------------------------------------------------------
;;;                       Ruby/Rails setup
;;; -----------------------------------------------------------------
(dolist (pattern '("\\.rb$" "\\.rake$" "Rakefile$" "\\.gemspec$" "Gemfile$" "Capfile"))
  (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

(eval-after-load 'rinari
  '(progn
     (message "Rinari loaded")
     ))

(defun ffy-insert-ruby-string-interpolation ()
  "In a double quoted string, interpolation is inserted on #."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (inf-ruby-setup-keybindings)
     (define-key ruby-mode-map [(return)] 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map [(?#)] 'ffy-insert-ruby-string-interpolation)
     (define-key ruby-mode-map [(control ?h) ?r] 'yari)))

(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ffy-run-programming-hook)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'scss-mode-hook 'flymake-sass-load)
(add-hook 'haml-mode-hook 'flymake-haml-load)

(provide 'init-prog)
