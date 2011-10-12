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

(provide 'init-org)
