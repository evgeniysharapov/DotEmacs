;;; package --- summary
;;; Configuration for personal MacBook Pro
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (progn
            (add-to-list 'exec-path-from-shell-variables "GOPATH")
            (exec-path-from-shell-initialize)))

(defvar my/font-height 150)
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


(provide 'Evgeniys-MBP)
;;; Evgeniys-MBP ends here
