;;; package --- summary
;;; Configuration for MacBook
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (progn
            (add-to-list 'exec-path-from-shell-variables "GOPATH")
            (exec-path-from-shell-initialize)))

(add-to-list 'default-frame-alist '(font . "Iosevka-14"))

(bind-key "<home>" 'beginning-of-visual-line)
(bind-key "<end>" 'end-of-visual-line)



