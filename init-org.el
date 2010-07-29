;;  
;;  New Emacs Configuration
;;  Org Mode 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun outline-down-heading ()
  "Goes to the next heading with one level up, unless there's none, then 
it goes to the next heading"
  (interactive)
  )

(defun sen/org-mode-custom-bindings ()
  "customize org-mode keys"
  (local-set-key [(control up)] 'outline-previous-visible-heading)
  (local-set-key [(control down)]  'outline-next-visible-heading)
  (local-set-key [(control meta up)]  'outline-up-heading)
  (local-set-key [(control c) (meta ?w)] 'org-store-link )
  (local-set-key [(control c) (control ?y)] 'org-insert-link)
  )

(defun sen/org-mode-yasnippet-fix ()
  ;; (yas/initialize)
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (define-key yas/keymap [(tab)] 'yas/next-field-group))

;;
;; Adding functionality to integrate with Outlook
;;
(require 'org-outlook)


(add-hook 'org-mode-hook
   (lambda ()
      ; To follow links with RET
     (setq org-return-follows-link t
           org-completion-use-ido t)
     (sen/org-mode-custom-bindings)
     (sen/org-mode-yasnippet-fix)
     (yas/minor-mode-on)
     (hl-line-mode +1)
     (turn-on-auto-fill)))


(provide 'init-org)
