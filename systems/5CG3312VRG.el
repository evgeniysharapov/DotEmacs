;;; package --- summary
;;; Configuration for LabCorp WSL
;;; Commentary:
;;; Code:

(defvar my/font-height 140)
(defvar my/font-family "Iosevka")

(add-hook 'after-init-hook
          (lambda nil
            (set-face-attribute
             'default nil
             :family my/font-family
             :height my/font-height
             :weight 'normal)
            ))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;
;; This runs in WSL2 environment and there's an issue with the clipboard
;; looks like it affects only emacs29
(defun my-trace-advice (orig-fun &rest data)
  ;;(message "pgtk-own-selection-internal called with args %S" (string= (car data) "PRIMARY"))
  (let ((res
         (if (string= (car data) "PRIMARY")
             t
           (apply orig-fun data))
         ))
    res))

(advice-add 'pgtk-own-selection-internal :around #'my-trace-advice)


(provide '5CG3312VRG)
;;; 5CG3312VRG ends here

