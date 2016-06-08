(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/data/bookmarks")
 '(byte-compile-verbose nil)
 '(color-theme-is-cumulative t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(custom-buffer-done-kill t)
 '(custom-safe-themes
   (quote
    ("dad07f6be6fbf4779d0d2ff58bb27014a7ab9088cf151a41cad088c3630937af" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "f4b7b67590ade2debce906caf4093d5b3aa2b4a365e46fbfba9965c470ed6d6c" "f6ad6c31eabd3d107621127fa55d9f053e1bc59efdc95febfe8b74e592ac63e9" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "2b2fff94a0e7e4f46d46b6cb072d43005a84460f6f812c5e63e0ec9e23b36ba0" "030bed79e98026124afd4ef8038ba7fe064314baf18b58759a5c92b91ec872fb" "e9f642ee0dbd5638e40390b8b8eded9743f1426ad1390e7b2e5d3fa04efa2969" "9bc6cf0c6a6c4b06b929e8cd9952478fa0924a4c727dacbc80c3949fc0734fb9" "7d833883f82f10855ae65ca1e5386916971d16cfcad3c4c3cc358ae37538cd5e" "cc0e791b1402636808924566a218e7d239f3c3be4cff3f3a9cf699130723957d" "5de500cbca775a5328efc17598c42e96ee475b0f92b1da84a6596570cc7402c4" "a284400e4b6d16eb5dd004c18dbe9557312f8cc9a13aa48d7cd5079f5147f8f8" "058c8fecde91b97c6bcc4f23159d09e3c116886fce505bdcf45b71bd68d26b0e" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "028a865f20dc76715d49656bec1965e1c2fff017fefedfd0c405fc7905e1e1ba" "7fe1e3de3e04afc43f9a3d3a8d38cd0a0efd9d4c" "d14db41612953d22506af16ef7a23c4d112150e5" default)))
 '(custom-theme-directory "~/.emacs.d/site-lisp/themes")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-globals-to-save
   (quote
    (desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring regexp-history register-alist file-name-history extended-command-history grep-history ido-last-directory-list minibuffer-history query-replace-history shell-command-history)))
 '(desktop-save-mode t)
 '(echo-keystrokes 0.1)
 '(enable-recursive-minibuffers t)
 '(flyspell-use-meta-tab nil)
 '(font-lock-verbose nil)
 '(gc-cons-threshold 4000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(haskell-tags-on-save t)
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs try-complete-file-name-partially try-complete-file-name try-expand-list try-expand-line)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-max-prospects 10)
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-silently-savep t)
 '(kept-new-versions 3)
 '(kill-do-not-save-duplicates t)
 '(magit-diff-refine-hunk t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-yank-at-point t)
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(org-agenda-files (quote ("~/org/stoneriver.org")))
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(require-final-newline t)
 '(rspec-use-rvm t)
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-language-standard . c++11)
     (flycheck-clang-language-standard . c++11)
     (scss-compile-at-save)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(save-place t nil (saveplace))
 '(savehist-mode t nil (savehist))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-margin 5)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(tab-always-indent (quote complete))
 '(tags-revert-without-query t)
 '(undo-limit 2000000)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(user-full-name "Evgeniy Sharapov")
 '(user-mail-address "evgeniy.sharapov@gmail.com")
 '(visible-bell t)
 '(winner-mode t nil (winner))
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(error ((t (:underline (:color "red1" :style wave)))))
 '(font-lock-warning-face ((t (:underline (:color "orange1" :style wave)))))
 '(js2-warning ((t (:underline (:color "orange" :style wave)))))
 '(magit-item-highlight ((t (:inherit hl-line))))
 '(warning ((t (:foreground "#ce5c00" :underline t)))))
