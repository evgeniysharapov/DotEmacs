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
  )

(defun sen/org-mode-yasnippet-fix ()
  (yas/initialize)
  (org-set-local 'yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-group)
  )

(add-hook 'org-mode-hook 'sen/org-mode-custom-bindings)
(add-hook 'org-mode-hook 'sen/org-mode-yasnippet-fix)
(add-hook 'org-mode-hook 'yas/minor-mode)

(add-hook 'org-mode-hook (lambda ()
                           (setq org-return-follows-link t)  ; To follow links with RET
                           ))


(provide 'init-org)
