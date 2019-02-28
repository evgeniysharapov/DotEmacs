;;; Feast For Eyes theme --- Emacs theme that is tailored to work in GUI and Terminal
;;
;;
;; Author: Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;; Version: 0.0.1
;; Keywords: themes light
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;; Licenese:
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; An Emacs port of the Atom Dark theme from Atom.io.
;;
;;; Code:
(require 'color)
(deftheme feast-for-eyes
  "Feast For Eyes theme --- Emacs theme that is tailored to work in GUI and Terminal")

(setq )


(custom-theme-set-variables
 'FeastForEyes
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 10)
 '(custom-buffer-done-kill t)
 '(default-input-method "russian-computer")
 '(desktop-restore-eager 2)
 '(desktop-restore-frames nil)
 '(directory-free-space-args "-Pmh")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhG")
 '(echo-keystrokes 0.1)
 '(eldoc-idle-delay 0)
 '(enable-recursive-minibuffers t)
 '(gc-cons-threshold 104857600)
 '(global-auto-revert-non-file-buffers t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 24 -1) " " filename))))
 '(ibuffer-jump-offer-only-visible-buffers t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("DaStEd" ((filename . "dasted"))) ("Kattis" ((filename . "Kattis"))) ("Mozart" ((filename . "Mozart\\.Hg"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-max-prospects 10)
 '(ido-mode (quote both))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(imenu-auto-rescan t)
 '(imenu-max-item-length 60)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen nil)
 '(initial-scratch-message nil)
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(load-prefer-newer t)
 '(mouse-yank-at-point t)
 '(org-confirm-babel-evaluate nil)
 '(org-hide-leading-stars t)
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(projectile-indexing-method (quote alien))
 '(projectile-sort-order (quote recentf))
 '(python-indent-guess-indent-offset nil)
 '(python-shell-prompt-block-regexp "\\s-+\\.\\.\\. ")
 '(python-shell-prompt-detect-failure-warning nil)
 '(safe-local-variable-values (quote ((mocha-project-test-directory . "test/server\"") (mocha-options . "--reporter spec --recursive --compilers js:babel-core/register") (mocha-project-test-directory . "test/server") (mocha-which-node) (js2-basic-offset . 2) (markdown-command . "pandoc -s --toc -S -c assets/style.css -B assets/before.html -A assets/after.html -f markdown -t html"))))
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables (quote (mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)))
 '(savehist-autosave-interval 60)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position 1)
 '(sentence-end-double-space nil)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(tags-revert-without-query t)
 '(user-full-name "Evgeniy N. Sharapov")
 '(user-mail-address "evgeniy.sharapov@gmail.com")
 '(visible-bell t)
 '(auto-image-file-mode t)
 '(column-number-mode t)
 '(global-auto-revert-mode t)
 '(mouse-avoidance-mode (quote animate))
 '(show-paren-mode t)
 '(which-function-mode t)
 '(winner-mode t)
 '(package-selected-packages (quote (dracula-theme spacemacs-theme espresso-theme faff-theme flatui-theme nord-theme less-css-mode diminish outshine groovy-mode tern js2-mode multiple-cursors zop-to-char adaptive-wrap whitespace-cleanup-mode easy-kill visual-regexp nlinum ace-link ivy-pages reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree writeroom-mode golden-ratio ibuffer-vc focus-autosave-mode counsel ivy-hydra spaceline anzu bug-hunter paradox hydra which-key stripe-buffer page-break-lines solarized-theme osx-trash exec-path-from-shell validate cargo toml-mode racer flycheck-rust rust-mode json-navigator crosshairs ob-ipython ac-dabbrev web-mode ac-capf ac-anaconda auto-complete idle-highlight-mode rainbow-mode yasnippet yaml-mode wgrep-ag use-package undo-tree smex projectile paredit nginx-mode monky mocha markdown-mode magit json-rpc json-mode js-comint imenu-list imenu+ ido-ubiquitous hide-lines help-mode+ help-fns+ help+ go-eldoc flycheck-pos-tip flx-ido find-file-in-project elisp-slime-nav dockerfile-mode docker-tramp docker company-tern company-statistics company-go company-c-headers company-anaconda color-moccur cider c-eldoc browse-kill-ring+ aok all-the-icons ace-window ace-jump-mode 4clojure)))
 '(custom-safe-themes (quote ("a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "eae43024404a1e3c4ea853a9cf7f6f2ad5f091d17234ec3478a23591f25802eb" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default)))
 '(custom-theme-directory "~/.emacs.d/theme"))

(custom-theme-set-faces
 'FeastForEyes
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:italic t :bold t))))
 '(info-node ((t (:italic t :bold t))))
 '(info-xref ((t (:bold t))))
 '(italic ((t (:italic t))))
 '(underline ((t (:underline t))))
 '(fringe ((t (:background "#f7f7f7"))))
 '(mode-line ((t (:background "#e8e8e8" :foreground "black" :box (:line-width -1 :color "#a5a5a5")))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#9599B0")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#f5f5f5" :foreground "grey20" :box (:line-width -1 :color "#a5a5a5") :weight light))))
 '(show-paren-match ((t (:background "#AAAAAA"))))
 '(show-paren-mismatch ((t (:foreground "#F9F2CE" :background "#F93232"))))
 '(hl-line ((t (:background "#EEEEEE"))))
 '(highlight ((t (:background "#EEE00A"))))
 '(isearch ((t (:background "#EEE00A"))))
 '(lazy-highlight ((t (:background "#FBE9AD"))))
 '(region ((t (:background "ns_selection_color"))))
 '(secondary-selection ((t (:background "#FBE9AD"))))
 '(shadow ((t (:foreground "grey50" :background "#EEEEEE"))))
 '(font-lock-builtin-face ((t (:foreground "#626FC9"))))
 '(font-lock-comment-face ((t (:foreground "#7F7F7F"))))
 '(font-lock-constant-face ((t (:foreground "#7653C1" :background "#F3F2FF"))))
 '(font-lock-function-name-face ((t (:foreground "#4E279A"))))
 '(font-lock-keyword-face ((t (:foreground "#6700B9"))))
 '(font-lock-preprocessor-face ((t (:foreground "#434343"))))
 '(font-lock-string-face ((t (:foreground "#BC670F" :background "#FDFBF5"))))
 '(font-lock-type-face ((t (:foreground "#699D36"))))
 '(font-lock-variable-name-face ((t (:foreground "#7B8C4D"))))
 '(font-lock-warning-face ((t (:foreground "#F93232"))))
 '(diff-file-header ((t (:bold t :inherit diff-header))))
 '(diff-header ((t (:background "#DDDDFF" :foreground "grey20"))))
 '(diff-added ((t (:background "#DDFFDD"))))
 '(diff-removed ((t (:background "#FFDDDD"))))
 '(diff-changed ((t (:background "#FFFFDD"))))
 '(diff-refine-changed ((t (:background "#DDDDFF"))))
 '(default ((t (:backround "grey99")))))


(provide-theme 'feast-for-eyes)
;;; feast-for-eyes-theme.el file ends here

