;;  
;;  New Emacs Configuration
;;  Setup for a Editing Text Modes (XML, Org, Text, etc.)
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; ------------------------------------------------------------
;;     XSL/XML setup.
;; ------------------------------------------------------------
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

;; another way to recognize XML files 
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(defun ffy-customize-nxml-mode ()
  "This function sets some variables and calls some functions that setup nXML mode."
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
  (setq ido-use-filename-at-point nil))

(add-hook 'nxml-mode-hook 'ffy-customize-nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
               ""
               nil))
;;; ----------------------------------------------------------------------
;;;  HTML and XHTML setup
;;; ----------------------------------------------------------------------

;; ------------------------------------------------------------
;;                      Org Mode
;; ------------------------------------------------------------
; '(org-agenda-include-diary t)
; '(org-clock-clocktable-default-properties (quote (:maxlevel 4 :scope file :step week :block thisweek :tend "")))
; '(org-empty-line-terminates-plain-lists t)
; '(org-hide-leading-stars t)
; '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-id org-info org-jsinfo org-special-blocks org-w3m org-panel)))
; '(org-return-follows-link t)
; '(org-use-sub-superscripts (quote {}))

(setq org-completion-use-ido t
      org-hide-leading-stars t
      org-return-follows-link t
      org-modules '(org-docview org-gnus org-id org-info org-jsinfo org-protocol org-special-blocks org-w3m org-bookmark org-elisp-symbol org-panel)
      org-empty-line-terminates-plain-lists t)


;(setq org-todo-keyword-faces
;      (quote (("TODO" :foreground "medium blue" :weight bold)
;              ("NOTE" :foreground "dark violet" :weight bold)
;              ("STARTED" :foreground "dark orange" :weight bold)
;              ("WAITING" :foreground "red" :weight bold)
;              ("DELEGATED" :foreground "red" :weight bold))))

;(defun my-org-mode-custom-bindings ()
;  "customize org-mode keys"
;  (local-set-key [(control up)] 'outline-previous-visible-heading)
;  (local-set-key [(control down)]  'outline-next-visible-heading)
;  (local-set-key [(control meta up)]  'outline-up-heading)
;  (local-set-key [(control c) (meta ?w)] 'org-store-link )
;  (local-set-key [(control c) (control ?y)] 'org-insert-link)
;  (local-set-key [(control c) ?a] 'org-agenda))


(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'yas-minor-mode-on)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'hl-line-mode)
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



;;; Add Markdown mode specifics.
;;; make outline-mode navigation work for underline headers as well
(add-hook 'markdown-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "#+\\|^\\(.*\\)\n\\(===+\\|---+\\)$")))

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

(defun turn-on-electric-mode ()
  (electric-pair-mode +1))

(defun ffy-programming-keys ()
  "Sets some keys for the programming mode:
    RET - is newline-and-indent"
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(shift return)] 'open-line))

(dolist   (it '(local-column-number-mode
                local-comment-auto-fill
                turn-on-hl-line-mode
                pretty-greek
                prog-mode-faces-add
                turn-on-flyspell-prog-mode
                turn-on-flymake
                ffy-programming-keys))
  (if (fboundp it)
      (add-hook '*programming-hook* it)))

(defun ffy-run-programming-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks '*programming-hook*))

;;; ------------------------------------------------------------
;;;                        Auto-Complete
;;; ------------------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *data-dir* "ac-dict"))
(setq ac-comphist-file (concat *data-dir* "ac-comphist.dat"))
(ac-config-default)
(setq ac-dwim t
      ac-auto-start t)

;;;;;;;;;;;;;;; Specific Programming Languages Environment Follow ;;;;;;;;;

;; --------------------------------------------------
;;    Lisp-like Programming Languages
;; --------------------------------------------------
(defconst *emacs-lisp-modes* '(emacs-lisp-mode lisp-mode ielm-mode))
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
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(eval-after-load "ac-nrepl"
  '(progn (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
          (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
          (add-to-list 'ac-modes 'nrepl-mode)))

(defun ffy-find-file-in-clojure-project ()
  "For Clojure we are also looking for project.clj file in the project root"
  (progn
    (require 'find-file-in-project)
    (when (boundp 'ffip-project-file)
      (set (make-local-variable 'ffip-project-file)
           (if (listp 'ffip-project-file)
               (cons "project.clj" ffip-project-file)
             (list "project.clj" ffip-project-file))))))

(add-hook 'clojure-mode-hook 'ffy-find-file-in-clojure-project)

;;; -----------------------------------------------------------------
;;;                       Ruby/Rails setup
;;; -----------------------------------------------------------------
(dolist (pattern '("\\.rb$" "\\.rake$" "Rakefile$" "\\.gemspec$" "Gemfile$" "Capfile"))
  (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(global-rinari-mode 1)

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
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
;;; -----------------------------------------------------------------
;;;                       SCSS/SASS setup
;;; -----------------------------------------------------------------
(add-hook 'scss-mode-hook 'ffy-run-programming-hook)
(add-hook 'sass-mode-hook 'ffy-run-programming-hook)
;;; -----------------------------------------------------------------
;;;                       Scala setup
;;; -----------------------------------------------------------------

;;; -----------------------------------------------------------------
;;;                           Octave Mode
;;; -----------------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook (lambda ()
                              (auto-fill-mode 1)))
;;; -----------------------------------------------------------------
;;;                           Haskell Mode
;;; -----------------------------------------------------------------
(add-hook 'haskell-mode-hook 'ffy-run-programming-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


(provide 'custom-modes)
