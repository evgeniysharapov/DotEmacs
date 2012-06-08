;;  
;;  New Emacs Configuration
;;  Setup for a programming language modes 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defvar *programming-hook* nil "This variable contains functions that we need to run if we are programming ")
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
  (font-lock-add-keywords
   nil
   '(("\\s<\\s-*\\(FIX\\(ME\\)?\\|TODO\\|XXX\\):?\\(.+\\)\\>" 1 'prog-mode-watchword-face t)
     ("\\s<\\s-*\\([Bb][Uu][Gg]\\s-+[0-9]+\\)\\s-*:?"  1 'prog-mode-bugs-face t))))

(add-hook '*programming-hook* 'local-column-number-mode)
(add-hook '*programming-hook* 'local-comment-auto-fill)
(add-hook '*programming-hook* 'turn-on-hl-line-mode)
(if (fboundp 'pretty-greek)
    (add-hook '*programming-hook* 'pretty-greek))
(add-hook '*programming-hook* 'prog-mode-faces-add)
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

;;; Set up Lisp keys 
(define-key lisp-mode-shared-map [(return)] 'reindent-then-newline-and-indent)

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

;;; ------------------------------------------------------------
;;;                        Auto-Complete
;;; ------------------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *dotfiles-dir* "ac-dict"))
(ac-config-default)
(setq ac-dwim t
      ac-auto-start t)

;;; -----------------------------------------------------------------
;;;   Ruby/Rails setup
;;; -----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;
;; Some settings and functions for XML setup.
;; 
;; Evgeniy Sharapov 
;;
(defun xml-pretty-print (begin end)
  "Makes current buffer with XML markup look prettier"
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (xml-pretty-print begin end))

(defun xml-pretty-print-buffer ()
  "Formats whole buffer containing XML"
  (interactive)
  (xml-pretty-print-region (point-min) (point-max)))

(setq-default
 ;; Treat elements and contents like S-expressions! Oh, the magic. 
 ;; (if you know S-expression movement commands, it's great) 
 nxml-sexp-element-flag t
  ;; Whenever you type </ it will fill out the rest. 
 nxml-slash-auto-complete-flag t)


;; Causes files with extensions .xml .xsl .rng .xhtml .html and .tal
;; to invoke nxml-mode.
(setq auto-mode-alist 
      (cons '("\\.\\(xml\\|xsl\\|rng\\|tal\\|xsd\\|sch\\|xslt\\|svg\\|rss\\)\\'" . nxml-mode) 
            (remove-if (lambda (x) (eq (cdr x) 'html-mode)) auto-mode-alist)))
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;; another way to recognize XML files 
(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(add-hook 'nxml-mode-hook '(lambda () (progn
                                   ;; load hide show modes 
                                   (local-set-key "\C-c/" 'nxml-finish-element)
                                   (local-set-key [return] 'newline-and-indent)
                                   ;;(auto-fill-mode)
                                   (rng-validate-mode)
                                   (unify-8859-on-decoding-mode)
                                   (setq ispell-skip-html t)
                                   (hs-minor-mode 1)
                                   ;; controversial 
                                   (make-variable-buffer-local 'ido-use-filename-at-point)
                                   (setq ido-use-filename-at-point nil)
                                   )))

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
               ""
               nil))

;;  
;;  New Emacs Configuration
;;  Org Mode 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "medium blue" :weight bold)
              ("NOTE" :foreground "dark violet" :weight bold)
              ("STARTED" :foreground "dark orange" :weight bold)
              ("WAITING" :foreground "red" :weight bold)
              ("DELEGATED" :foreground "red" :weight bold))))

(defun my-org-mode-custom-bindings ()
  "customize org-mode keys"
  (local-set-key [(control up)] 'outline-previous-visible-heading)
  (local-set-key [(control down)]  'outline-next-visible-heading)
  (local-set-key [(control meta up)]  'outline-up-heading)
  (local-set-key [(control c) (meta ?w)] 'org-store-link )
  (local-set-key [(control c) (control ?y)] 'org-insert-link)
  (local-set-key [(control c) ?a] 'org-agenda))

(defun my-org-mode-yasnippet-fix ()
  ;; (yas/initialize)
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (define-key yas/keymap [(tab)] 'yas/next-field-group))


(add-hook 'org-mode-hook
   (lambda ()
     (my-org-mode-custom-bindings)
     (my-org-mode-yasnippet-fix)
     (when (fboundp 'yas/minor-mode-on)
         (yas/minor-mode-on))
     (hl-line-mode +1)
     (turn-on-auto-fill)))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;;; Fix conflict of Windmove and Org mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;
;;  Setup iimage working with Org-mode
;; 
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))


(provide 'init-edit-modes)
