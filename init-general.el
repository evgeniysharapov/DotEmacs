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

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t)

;;
;; Initialize Color-Theme
;;
(if (fboundp 'color-theme-initialize)
	(progn 
	 (setq color-theme-is-global nil)
	 (color-theme-initialize)))

(display-time)
;; --------------------------------------------------
;;        Files Settings and Operations 
;; --------------------------------------------------

;; Save a list of recent files visited.
(recentf-mode 1)

;;  File associations 
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; Backups and saves 
(setq save-place-file (concat dotfiles-dir "places"))
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; --------------------------------------------------
;;                    Byte Compilation 
;; --------------------------------------------------
(setq byte-compile-verbose nil)
(setq byte-compile-warnings t)
(setq byte-cache-directory nil)
(setq bcc-enabled-on-save t)
(setq bcc-blacklist '("/\\.recentf$" "/history$" "/\\.ecb-user-layouts\\.el$" "/\\.session$" "/\\.emacs-project$" "/\\.emacs\\.desktop$" "/custom\\.el$" "/init\\.el$" "/\\.ido\\.last$" "/\\.ecb-tip-of-day\\.el$" "/\\.viper$" "/\\.recentf$" "cal-loaddefs\\.el" "esh-groups\\.el"))
(require 'byte-code-cache nil t)

;; --------------------------------------------------
;;                    Buffers 
;; --------------------------------------------------

;; Encoding and text related stuff 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq uniquify-buffer-name-style 'forward
      mouse-yank-at-point t
      require-final-newline t
      delete-selection-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(defun my/move-line-up ()
  "Move curent line up one line"
  (interactive)
  (transpose-lines 1)
  (previous-line)
  (previous-line))

(defun my/move-line-down ()
  "Move curent line down one line"
  (interactive)
  (next-line)
  (transpose-lines 1)
  (previous-line))

(global-set-key [(control ?x) (control up)] 'my/move-line-up)
(global-set-key [(control ?x) (control down)] 'my/move-line-down)


;; --------------------------------------------------
;;   Windows and stuff 
;; --------------------------------------------------
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; --------------------------------------------------
;;    Miscellaneous 
;; --------------------------------------------------
;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(setq redisplay-dont-pause t)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

;;;; Setup IDO mode 
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-show-dot-for-dired t)

;;;; add Smex 
(if (fboundp 'smex-initialize)
    (progn 
      (smex-initialize)
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

;;;; add Company completion

(provide 'init-general)
