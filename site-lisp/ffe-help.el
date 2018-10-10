;;; Help/Info configuration
(require 'subr-x)
(use-package help-mode+)
(use-package help+)
(use-package help-fns+)

;; How often I need to see emacs FAQ. Use C-h C-f as a keymap for find-* commands
(defvar help-find-map)
(define-prefix-command 'help-find-map)
(bind-key "C-f" 'help-find-map help-map)

;; I don't really need to see view-hello-file either 
(defvar help-help-map)
(define-prefix-command 'help-help-map)
(bind-key "h" 'help-help-map help-map)


(use-package help
  :config (progn
            (defun ffe-describe-symbol-at-point ()
              "Describes symbol at a point"
              (interactive)
              (when-let (symbol (symbol-at-point))
                (describe-symbol symbol)))

            (defun ffe-quit-windows-on-help ()
              "Quits window with *Help* buffer"
              (interactive)
              (quit-windows-on "*Help*")))  
  :bind (:map help-map
              ("C-b" . describe-personal-keybindings)
              ("C-k" . describe-key-briefly)
              ("C-c" . describe-char)
              ("C-s" . ffe-describe-symbol-at-point)
              ("C-S-s" . describe-symbol)
              ;; Too often I need to close Help windows after seeing the doc and I don't really use `help-quit' anyway
              ([remap help-quit] . ffe-quit-windows-on-help)
              ;; help-for-help is nice, but I would like C-h still show me prefix bindings
              ("C-h" . describe-prefix-bindings)
              
         ;; finding a variable or a function or a library, i.e. opening them in the editor
         :map help-find-map
              ("f" . find-function)
              ("v" . find-variable)
              ("l" . find-library)))

(use-package helpful
  :ensure t
  :bind (:map help-help-map
              ("f" . helpful-function)
              ("v" . helpful-variable)
              ("c" . helpful-command)
              ("m" . helpful-macro)
              ("h" . helpful-at-point)))

(provide 'ffe-help)
