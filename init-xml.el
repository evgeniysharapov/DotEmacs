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

(provide 'init-xml)
