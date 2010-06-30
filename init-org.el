;;  
;;  New Emacs Configuration
;;  Org Mode 
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun sen/org-mode-custmo-bindings ()
  "customize org-mode keys"
  (local-set-key [(control up)] 'outline-previous-visible-heading)
  (local-set-key [(control down)]  'outline-next-visible-heading)
  (local-set-key [(control meta up)]  'outline-up-heading)
  )

(defun sen/org-mode-yasnippet-fix ()
  (yas/initialize)
  (org-set-local 'yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-group)
  )

(add-hook 'org-mode-hook 'sen/org-mode-custmo-bindings)
(add-hook 'org-mode-hook 'sen/org-mode-yasnippet-fix)

(provide 'init-org)
