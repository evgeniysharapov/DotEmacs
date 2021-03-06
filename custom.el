(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(anaconda-mode-eldoc-as-single-line t)
 '(auto-image-file-mode t)
 '(aw-dispatch-always t)
 '(aw-ignore-current nil)
 '(aw-keys '(108 107 106 104 103 102 100 115 97))
 '(aw-scope 'frame)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag t)
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 10)
 '(custom-buffer-done-kill t)
 '(custom-safe-themes
   '("2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" default))
 '(default-input-method "russian-computer")
 '(delete-old-versions t)
 '(desktop-globals-to-save
   '(desktop-missing-file-warning
     (search-ring . 50)
     (regexp-search-ring . 50)
     (regexp-history . 50)
     (grep-history . 50)
     register-alist file-name-history tags-file-name
     (shell-command-history . 50)
     (read-expressions-history . 50)
     (query-replace-history 0.5)
     (minibuffer-history . 50)
     (compile-history . 50)))
 '(desktop-restore-eager 2)
 '(desktop-restore-frames nil)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(directory-free-space-args "-Pmh")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhG")
 '(echo-keystrokes 0.1)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-fsharp-server-runtime 'net-core)
 '(eglot-fsharp-server-version "0.45.4")
 '(eldoc-idle-delay 0)
 '(enable-recursive-minibuffers t)
 '(gc-cons-threshold 104857600)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(groovy-highlight-assignments t)
 '(history-delete-duplicates t)
 '(history-length 2000)
 '(ibuffer-expert t)
 '(ibuffer-formats
   '((mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 24 -1)
           " " filename)))
 '(ibuffer-jump-offer-only-visible-buffers t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("DropboxNotes"
      (filename . "Dropbox/Notes"))
     ("DaStEd"
      (filename . "dasted"))
     ("Kattis"
      (filename . "Kattis"))
     ("Mozart"
      (filename . "Mozart\\.Hg"))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))
     ("programming"
      (or
       (mode . emacs-lisp-mode)
       (mode . cperl-mode)
       (mode . c-mode)
       (mode . java-mode)
       (mode . idl-mode)
       (mode . lisp-mode)))))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-max-prospects 10)
 '(ido-mode 'both nil (ido))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point 'guess)
 '(imenu-auto-rescan t)
 '(imenu-list-focus-after-activation t)
 '(imenu-max-item-length 60)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen nil)
 '(initial-scratch-message nil)
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(kill-whole-line t)
 '(ledger-binary-path "ledger")
 '(load-prefer-newer t)
 '(ls-lisp-dirs-first t)
 '(monky-process-type 'cmdserver)
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(mouse-yank-at-point t)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("w" "Work TODOs " alltodo ""
      ((org-agenda-files
        '("~/ai-worklog/Worklog.org" "~/ai-worklog/PULSE.org" "~/ai-worklog/ULP.org" "~/ai-worklog/AOD_Service.org" "~/ai-worklog/DHIN_Closure.org" "~/ai-worklog/Management_Reports.org"))))))
 '(org-agenda-files
   '("~/Dropbox/Notes/Notes.org" "~/Dropbox/Notes/Cooking.org" "~/Dropbox/Notes/Development.org" "~/Dropbox/Notes/Docker_Workshop.org" "~/Dropbox/Notes/Health.org" "~/Dropbox/Notes/Ideas.org" "~/Dropbox/Notes/Interviews.org" "~/Dropbox/Notes/Journal.org" "~/Dropbox/Notes/Links.org" "~/ai-worklog/Management_Reports.org" "~/Dropbox/Notes/Money.org" "~/ai-worklog/PULSE.org" "~/Dropbox/Notes/Setup.org" "~/Dropbox/Notes/Todo.org" "~/ai-worklog/ULP.org" "~/ai-worklog/Worklog.org"))
 '(org-capture-templates
   '(("w" "Work Task" entry
      (file+olp+datetree "~/ai-worklog/Worklog.org")
      "* %^{Description}  %^g

%?" :clock-in t :clock-keep t)
     ("W" "Work Task (with ID)" entry
      (file+olp+datetree "~/ai-worklog/Worklog.org")
      "* %^{Description}  %^g
:PROPERTIES:
:ID:       %(ffe-uuid)
:CREATED:  %U
:END:

%?" :clock-in t :clock-keep t)
     ("a" "Add Task" entry
      (file+headline "~/Dropbox/Notes/Todo.org" "Inbox")
      "* TODO %?
:PROPERTIES:
:ID:       %(ffe-uuid)
:CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/Notes/Notes.org")
      "* NOTE %? %^G
:PROPERTIES:
:ID:       %(ffe-uuid)
:CREATED:  %U
:END:" :prepend t)
     ("p" "Protocol" entry
      (file+headline "~/Dropbox/Notes/Todo.org" "Inbox")
      "* NOTE %?
:PROPERTIES:
:ID:       %(ffe-uuid)
:CREATED:  %U
:URL:      %:link
:END:

%i
")
     ("l" "Link" entry
      (file+headline "~/Dropbox/Notes/Todo.org" "Inbox")
      "* [[%:link][%:description]]
:PROPERTIES:
:ID:       %(ffe-uuid)
:CREATED:  %U
:URL:      %:link
:END:

#+BEGIN_QUOTE
%:initial
#+END_QUOTE

%?
")))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Dropbox/Notes/Notes.org")
 '(org-directory "~/Dropbox/Notes")
 '(org-export-backends '(ascii html icalendar latex md odt confluence))
 '(org-hide-leading-stars t)
 '(org-image-actual-width 600)
 '(org-link-abbrev-alist '(("jira" . "https://tracking.ainq.com/browse/%s")))
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-modules
   '(org-id ol-info org-mouse org-protocol org-tempo ol-eshell org-eval ol-git-link org-interactive-query org-toc org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-rmail org-tempo org-w3m))
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path "~/.bin/plantuml.jar")
 '(org-refile-use-outline-path 'file)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(outshine-fontify-whole-heading-line t)
 '(outshine-org-style-global-cycling-at-bob-p t)
 '(outshine-use-speed-commands t)
 '(package-selected-packages
   '(eglot fsharp-mode company-lsp modus-themes ob-restclient powerline powershell sass-mode merlin tuareg lsp-ui lsp-mode visual-regex company-yasnippet go-guru edit-indirect less-css-mode tern multiple-cursors zop-to-char adaptive-wrap whitespace-cleanup-mode easy-kill visual-regexp nlinum ace-link ivy-pages reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree writeroom-mode golden-ratio ibuffer-vc focus-autosave-mode counsel ivy-hydra spaceline anzu bug-hunter paradox hydra which-key stripe-buffer page-break-lines solarized-theme osx-trash validate toml-mode flycheck-rust crosshairs ob-ipython ac-dabbrev web-mode ac-capf ac-anaconda auto-complete idle-highlight-mode smex nginx-mode monky mocha json-rpc json-mode imenu-list imenu+ ido-ubiquitous hide-lines help-mode+ help-fns+ help+ go-eldoc flycheck-pos-tip flx-ido docker-tramp docker company-tern company-statistics company-go color-moccur c-eldoc browse-kill-ring+ aok all-the-icons ace-window ace-jump-mode 4clojure lua-mode visual-regex company-yasnippet go-guru edit-indirect less-css-mode tern multiple-cursors zop-to-char adaptive-wrap whitespace-cleanup-mode easy-kill visual-regexp nlinum ace-link ivy-pages reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree writeroom-mode golden-ratio ibuffer-vc focus-autosave-mode counsel ivy-hydra spaceline anzu bug-hunter paradox hydra which-key stripe-buffer page-break-lines solarized-theme osx-trash validate toml-mode flycheck-rust crosshairs ob-ipython ac-dabbrev web-mode ac-capf ac-anaconda auto-complete idle-highlight-mode smex nginx-mode monky mocha json-rpc json-mode imenu-list imenu+ ido-ubiquitous hide-lines help-mode+ help-fns+ help+ go-eldoc flycheck-pos-tip flx-ido docker-tramp docker company-tern company-statistics company-go color-moccur c-eldoc browse-kill-ring+ aok all-the-icons ace-window ace-jump-mode 4clojure))
 '(projectile-indexing-method 'alien)
 '(projectile-sort-order 'recentf)
 '(python-indent-guess-indent-offset nil)
 '(python-shell-prompt-block-regexp "\\s-+\\.\\.\\. " t)
 '(python-shell-prompt-detect-failure-warning nil)
 '(regexp-search-ring-max 100)
 '(safe-local-variable-values
   '((eval outline-minor-mode t)
     (markdown-asymmetric-header . t)
     (mocha-project-test-directory . "test/server\"")
     (mocha-options . "--reporter spec --recursive --compilers js:babel-core/register")
     (mocha-project-test-directory . "test/server")
     (mocha-which-node)
     (js2-basic-offset . 2)
     (markdown-command . "pandoc -s --toc -S -c assets/style.css -B assets/before.html -A assets/after.html -f markdown -t html")))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
 '(savehist-autosave-interval 60)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position 1)
 '(search-ring-max 100)
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(tags-revert-without-query t)
 '(user-full-name "Evgeniy N. Sharapov")
 '(user-mail-address "evgeniy.sharapov@gmail.com")
 '(visible-bell t)
 '(which-function-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

