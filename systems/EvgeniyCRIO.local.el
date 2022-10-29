;;; package --- summary
;;; Configuration for CRIO MacBook Pro
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (progn
            (add-to-list 'exec-path-from-shell-variables "GOPATH")
            (exec-path-from-shell-initialize)))

(defvar my/font-height 190)
(defvar my/font-family "Iosevka Nerd Font")

(add-hook 'after-init-hook
          (lambda nil
            (set-face-attribute
             'default nil
             :family my/font-family
             :height my/font-height
             :weight 'normal)
            ))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Setup Org Agenda files

(defun crio-insert-edc-jira-link (number)
  "Inserts LINK to JIRA issue"
  (interactive "n")
  (insert (org-make-link-string (format "edc:%s" number) (format "EDC-%s" number))))

(bind-key "C-c l" #'crio-insert-edc-jira-link org-mode-map)

(provide 'EvgeniyCRIO.local)
;;; EvgeniyCRIO ends here

