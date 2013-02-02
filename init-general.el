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

(when (fboundp 'highlight-symbol-mode)
  (highlight-symbol-mode))

(display-time)


;;; Hide some modes from the mode-line
(when (fboundp 'diminish)
  (eval-after-load 'eldoc
    '(diminish 'eldoc-mode))
  (eval-after-load 'undo-tree
    '(diminish 'undo-tree-mode)))

;; --------------------------------------------------
;;        Files Settings and Operations
;; --------------------------------------------------
;; Backups and saves
(setq save-place-file (concat *dotfiles-dir* "places")
      backup-directory-alist `(("." . ,(expand-file-name (concat *dotfiles-dir* "backups")))))


;; --------------------------------------------------
;;                    Byte Compilation
;; --------------------------------------------------
(add-hook 'after-save-hook (lambda ()
                             (when (eq major-mode 'emacs-lisp-mode)
                               (emacs-lisp-byte-compile-and-load))))


;; --------------------------------------------------
;;                    Buffers
;; --------------------------------------------------

;; Encoding and text related stuff
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)


(set-default 'imenu-auto-rescan t)

(defadvice previous-buffer (after my/previous-buffer activate)
  (ffy-display-prev-next-buffers))

(defadvice next-buffer (after my/next-buffer activate)
 (ffy-display-prev-next-buffers))

;;; Zap-up-to char is a better alternative
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)" 'interactive)

;;; ------------------------------------------------------------
;;; Undo settings
;;; ------------------------------------------------------------
(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode))

;;; ------------------------------------------------------------
;;;  Spellcheck setup 
;;; ------------------------------------------------------------
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; --------------------------------------------------
;;           Help and Info Functions
;; --------------------------------------------------
(require 'help-mode+ nil t)
(require 'help+ nil t)
(require 'help-fns+ nil t)

;; --------------------------------------------------
;;    Miscellaneous
;; --------------------------------------------------
(setq redisplay-dont-pause t)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(put 'set-goal-column 'disabled nil)

;;;; add Smex
(when (fboundp 'smex-initialize)
  (smex-initialize))

;;; Using smerge for merging files
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;;
;; Adding Yasnippets directory
;;
(if (and (file-directory-p (concat *dotfiles-dir* "snippets"))
         (fboundp 'yas/load-directory))
    (yas/load-directory (concat *dotfiles-dir* "snippets")))

;;; try to load bookmark+
(eval-after-load "bookmark"
  '(require 'bookmark+ nil t))

;;
;;  Specific settings for different OS
;;  point being that I have several computers running the same OS
;;  and it is quite tedious to copy/paste the same settings between
;;  files.

;; settings for Windows
(on-win32
  ;; maximize Emacs frame
  (global-set-key [(control f11)]
                  (make-interactive w32-send-sys-command #xf030 nil))
  ;; restore original size of the Emacs frame
  (global-set-key [(control shift f11)]
                  (make-interactive w32-send-sys-command #xf120 nil))
  ;; set font for the Windows
  (set-face-attribute 'default nil :family "Consolas" :height 100))


(provide 'init-general)
