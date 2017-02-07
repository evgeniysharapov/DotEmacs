(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anaconda-mode-eldoc-as-single-line t)
 '(auto-image-file-mode t)
 '(column-number-mode t)
 '(custom-buffer-done-kill t)
 '(default-input-method "russian-computer")
 '(desktop-restore-eager 2)
 '(desktop-restore-frames nil)
 '(directory-free-space-args "-Pmh")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhG")
 '(echo-keystrokes 0.1)
 '(eldoc-idle-delay 0)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(history-delete-duplicates t)
 '(ibuffer-expert t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   (quote
    (("Kattis"
      ((filename . "Kattis")))
     ("Mozart"
      ((filename . "Mozart\\.Hg")))
     ("gnus"
      ((or
	(mode . message-mode)
	(mode . mail-mode)
	(mode . gnus-group-mode)
	(mode . gnus-summary-mode)
	(mode . gnus-article-mode))))
     ("programming"
      ((or
	(mode . emacs-lisp-mode)
	(mode . cperl-mode)
	(mode . c-mode)
	(mode . java-mode)
	(mode . idl-mode)
	(mode . lisp-mode)))))))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-max-prospects 10)
 '(ido-mode (quote both) nil (ido))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(imenu-auto-rescan t)
 '(inhibit-startup-screen nil)
 '(initial-scratch-message nil)
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(load-prefer-newer t)
 '(ls-lisp-dirs-first t)
 '(monky-process-type (quote cmdserver))
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-yank-at-point t)
 '(org-confirm-babel-evaluate nil)
 '(org-hide-leading-stars t)
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (rainbow-mode yasnippet yaml-mode wgrep-ag use-package undo-tree smex projectile paredit nginx-mode monky mocha markdown-mode magit json-rpc json-mode js-comint imenu-list imenu+ ido-ubiquitous hide-lines help-mode+ help-fns+ help+ go-eldoc flycheck-pos-tip flx-ido find-file-in-project elisp-slime-nav dockerfile-mode docker-tramp docker company-tern company-statistics company-go company-c-headers company-anaconda color-moccur cider c-eldoc browse-kill-ring+ aok all-the-icons ace-window ace-jump-mode 4clojure)))
 '(python-indent-guess-indent-offset nil)
 '(safe-local-variable-values
   (quote
    ((mocha-project-test-directory . "test/server\"")
     (mocha-options . "--reporter spec --recursive --compilers js:babel-core/register")
     (mocha-project-test-directory . "test/server")
     (mocha-which-node)
     (js2-basic-offset . 2)
     (markdown-command . "pandoc -s --toc -S -c assets/style.css -B assets/before.html -A assets/after.html -f markdown -t html"))))
 '(save-interprogram-paste-before-kill t)
 '(scroll-preserve-screen-position t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tags-revert-without-query t)
 '(user-full-name "Evgeniy N. Sharapov")
 '(user-mail-address "evgeniy.sharapov@gmail.com")
 '(visible-bell t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:underline t :slant oblique))))
 '(flyspell-duplicate ((t (:underline (:color "Gold3" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "#0058B0"))))
 '(font-lock-comment-face ((t (:foreground "#948072" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#009B9B"))))
 '(font-lock-keyword-face ((t (:foreground "#720CAF" :weight semi-bold))))
 '(font-lock-string-face ((t (:foreground "#A02D05"))))
 '(font-lock-variable-name-face ((t (:foreground "#047B0D"))))
 '(js2-external-variable ((t (:inherit font-lock-variable-name-face :underline t)))))
