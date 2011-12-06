;;
;;  New Emacs Configuration
;;  General Settings ( Look and Feel, etc.)
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

;; --------------------------------------------------
;;           GUI/Look and Feel
;; --------------------------------------------------

;; Turn off bells and whistles
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; we want to know what is the file name
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(display-time)
;; --------------------------------------------------
;;        Files Settings and Operations
;; --------------------------------------------------

;;  File associations
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; Backups and saves
(setq save-place-file (concat *dotfiles-dir* "places"))
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat *dotfiles-dir* "backups")))))

;; --------------------------------------------------
;;                    Byte Compilation
;; --------------------------------------------------
(setq byte-compile-verbose nil)
(setq byte-compile-warnings (quote (unresolved obsolete suspicious)))
(setq byte-cache-directory nil)
(setq bcc-enabled-on-save t)
(setq bcc-blacklist '("/\\.recentf$" "/history$" "/\\.ecb-user-layouts\\.el$" "/\\.session$" "/\\.emacs-project$"
			"/\\.emacs\\.desktop$" "/custom\\.el$" "/init\\.el$" "/\\.ido\\.last$" "/\\.ecb-tip-of-day\\.el$"
			"/\\.viper$" "/\\.recentf$" "cal-loaddefs\\.el" "esh-groups\\.el"  "tramp-loaddefs\\.el" "finder-inf\\.el"))
(require 'byte-code-cache nil t)

;; --------------------------------------------------
;;                    Buffers
;; --------------------------------------------------

;; Encoding and text related stuff
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)


(set-default 'imenu-auto-rescan t)

;;;; After switching a buffer, display names of adjacent buffers in
;;;; the echo area.
;;;; From http://www.jurta.org/en/emacs/dotemacs
(defun my/display-prev-next-buffers ()
  "Show two previous, current and two next buffer names in the echo area.
Example:
-2:*Messages* -1:*Help*    0:.emacs      1:*info*  2:*scratch*"
  (interactive)
  (let ((i -3) b (bl (buffer-list (selected-frame))) (message-log-max nil))
    (message "%s"
             (mapconcat
              (lambda (x)
                (setq i (+ i 1))
                (format "%d:%-12s"
                        i (substring (buffer-name x) 0
                                     (min (length (buffer-name x)) 11))))
              (append
               (nreverse
                (list
                 (setq b (get-next-valid-buffer (reverse bl) t))
                 (get-next-valid-buffer (cdr (memq b (reverse bl))) t)))
               (list (current-buffer))
               (list
                (setq b (get-next-valid-buffer (cdr bl) t))
                (get-next-valid-buffer (cdr (memq b bl)) t)))
              " "))))

(defadvice previous-buffer (after my/previous-buffer activate)
  (my/display-prev-next-buffers))

(defadvice next-buffer (after my/next-buffer activate)
  (my/display-prev-next-buffers))

;;; Undo settings 

(autoload 'turn-on-undo-tree-mode "undo-tree" nil t)
(global-set-key [(control x) (control shift ?u)] 'turn-on-undo-tree-mode)

;;; ------------------------------------------------------------
;;;  Spellcheck setup 
;;; ------------------------------------------------------------
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; --------------------------------------------------
;;    Miscellaneous
;; --------------------------------------------------
(setq redisplay-dont-pause t)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(put 'set-goal-column 'disabled nil)

;;;; add Smex
(when (fboundp 'smex-initialize)
    (progn
      (smex-initialize)
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

;;; Using smerge for merging files
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

(provide 'init-general)
