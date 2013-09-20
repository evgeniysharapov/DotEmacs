;;
;;  New Emacs Configuration
;;  General Settings ( Look and Feel, etc.)
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(autoload '-difference "dash")
(autoload 's-lines "s")

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

(add-hook 'after-init-hook
          (lambda ()
            "Sets type font properties"
            (on-win32
             ;; set font for the Windows
             (set-face-attribute 'default nil :family "Consolas" :height 100))
            (on-mac
             ;; set font for Macs
             (when (window-system)
               (set-face-attribute 'default nil :family "Menlo" :height 110)))
            ;; (load-theme 'tomorrow-day t)
            ))

;;; Configure powerline if it is available
(when (fboundp 'powerline-default-theme)
  (powerline-default-theme))

;; --------------------------------------------------
;;        Files Settings and Operations
;; --------------------------------------------------
;; Backups and saves
(setq save-place-file (concat *data-dir* "places")
      backup-directory-alist `((".*" . ,*backup-dir*))
      savehist-file (concat *data-dir* "history")
      smex-save-file (concat *data-dir* ".smex-items")
      recentf-save-file (concat *data-dir* ".recentf")
      ido-save-directory-list-file (concat *data-dir* ".ido.last")
      bookmark-default-file (concat *data-dir* "bookmarks")
      desktop-dirname *data-dir*
      desktop-path (list desktop-dirname)
      desktop-save t
      auto-save-list-file-prefix (concat *data-dir* "auto-save-list/.saves-"))


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


(defvar *auto-close-buffers* '("*Completions*"
                               "*Ido Completions*")
  "List of buffers that should be closed after we done with minibuffer. Usually it is various completions buffers")

(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (progn
               (mapc '(lambda (buffer)
                        (if (buffer-live-p buffer)
                            (kill-buffer buffer))) *auto-close-buffers*))))

;;; enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'widen 'disabled nil)


;;; ------------------------------------------------------------
;;; Undo settings
;;; ------------------------------------------------------------
(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode))

;;; ------------------------------------------------------------
;;;  Spellcheck setup 
;;; ------------------------------------------------------------
(defun find-hunspell-dictionary ()
  "Searches for hunspell dictionaries using `hunspell -D' first and seeing if ther's
any dictionary found. If not then try to check if dictionary exist in the same directory (case for Windows).

On Mac OS X hunspell should search for dictionaries in at least /Library/Spelling. In fact, on Windows just drop dictionaries next to hunspell binary file.

It returns either nil or path to the dictionary that could be used with `hunspell -d'. Put it in the `ispell-extra-args' variable.

This function depends on 's and 'dash libraries."
  (when (executable-find "hunspell")
    ;; First, let's see if we can load any dicts by default
    (let* ((hunspell-output (shell-command-to-string "hunspell -D"))
           (hunspell-output-lines (remove-if #'(lambda (e) (equal e ""))
                                             (s-lines hunspell-output)))
           (loaded-dicts (member "LOADED DICTIONARY:"  hunspell-output-lines))
           (available-dicts (-difference (member-if #'(lambda (e)(s-starts-with? "AVAILABLE DICTIONARIES" e)) hunspell-output-lines)
                                         loaded-dicts)))
      ;; If we have loaded-dicts we should be fine, otherwise try to
      ;; search for dictionaries
      (unless
          (or (cdr loaded-dicts)
              ;; Could be a message:
              ;; Can't open affix or dictionary files for dictionary named
              ;; "default".
              (not (cdr available-dicts)))
        ;; let's check if there's dictionary next to the binary
        (let ((dictionary-path (concat
                                (file-name-directory
                                 (executable-find "hunspell")) "en_US")))
          (when (file-exists-p (concat dictionary-path ".dic"))
            dictionary-path))))))

(eval-after-load "ispell"
  '(progn
     ;; Personal dictionary setup
     (setq ispell-personal-dictionary (concat *data-dir* ".personal.dict"))
     ;; Aspell Specific
     (when (executable-find "aspell")
       (setq ispell-program-name "aspell"
             ispell-extra-args '("--sug-mode=ultra")))
     ;; Hunspell Specific
     (when (executable-find "hunspell")
       (setq ispell-program-name "hunspell")
       (let* ((dict-location (find-hunspell-dictionary)))
         (when dict-location
           (setq  ispell-extra-args '("-d" dict-location "-i" "utf-8")))))))

;; --------------------------------------------------
;;           Help and Info Functions
;; --------------------------------------------------
(require 'help-mode+ nil t)
(require 'help+ nil t)
(require 'help-fns+ nil t)

;; --------------------------------------------------
;;    Miscellaneous
;; --------------------------------------------------
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)


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

;;; ------------------------------------------------------------
;;; Some IDO settings that have been taken out from the customization file.
;;; ------------------------------------------------------------
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
;;; not every command should could be ido-ed
;;; kill-ring-search has already set of minibuffer commands that don't
;;; work well with ido-completing-read
(setq ido-ubiquitous-command-exceptions '(kill-ring-search))


;;; Dired settings that proved useful
(setq dired-dwim-target t)              ; guess where to copy files

;;; change some grep mode bindings
(eval-after-load "grep"
  '(progn
     (setq wgrep-enable-key "e")
     (define-key grep-mode-map [(?e)] 'wgrep-change-to-wgrep-mode)))

;;; try to load bookmark+
(eval-after-load "bookmark"
  '(require 'bookmark+ nil t))

;;; Add ThingAtPoint+
(eval-after-load "thingatpt"
  '(when (require 'thingatpt+)
     (tap-redefine-std-fns)))
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
  )


(provide 'custom-general)
