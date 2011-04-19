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


(add-hook 'org-mode-hook
   (lambda ()
      ; To follow links with RET
     (setq org-return-follows-link t
           org-completion-use-ido t)
     (sen/org-mode-custom-bindings)
     (sen/org-mode-yasnippet-fix)
     (when (fboundp 'yas/minor-mode-on)
         (yas/minor-mode-on))
     (hl-line-mode +1)
     (turn-on-auto-fill)))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;;
;;  Setup iimage working with Org-mode
;; 
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;; (add-hook 'org-mode-hook (lambda ()
;; 	(add-to-list 'iimage-mode-image-regex-alist
;;              (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
;;                            "\\)\\]")  1))))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))

(provide 'init-org)
