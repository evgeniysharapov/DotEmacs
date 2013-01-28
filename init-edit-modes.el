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
;; ------------------------------------------------------------
;;                      Org Mode
;; ------------------------------------------------------------
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

;;; Add Markdown mode specifics.
;;; make outline-mode navigation work for underline headers as well
(add-hook 'markdown-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "#+\\|^\\(.*\\)\n\\(===+\\|---+\\)$")))


(provide 'init-edit-modes)
