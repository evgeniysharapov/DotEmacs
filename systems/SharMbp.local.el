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

(set-face-attribute 'default nil :height 145)

(provide 'Evgeniys-MBP)
;;; Evgeniys-MBP ends here

