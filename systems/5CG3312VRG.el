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


(provide '5CG3312VRG)
;;; 5CG3312VRG ends here

