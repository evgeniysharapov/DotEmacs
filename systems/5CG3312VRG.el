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

;; This runs in WSL2 environment and there's an issue with the clipboard
(defun clipboard-copy (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (let* ((str (if region
                 (funcall region-extract-function nil)
               (filter-buffer-substring beg end)))
        (plain-string (substring-no-properties str)))
    (message (prin1-to-string  plain-string))
    (async-shell-command (concat "printf \"%s\" " (prin1-to-string plain-string)  " | clip.exe" ) nil)))

; (advice-add 'kill-ring-save  :after #'clipboard-copy)


(provide '5CG3312VRG)
;;; 5CG3312VRG ends here

