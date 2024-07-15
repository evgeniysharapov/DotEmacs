(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anaconda-mode-eldoc-as-single-line t)
 '(auto-image-file-mode t)
 '(aw-dispatch-always t)
 '(aw-ignore-current nil)
 '(aw-keys '(108 107 106 104 103 102 100 115 97))
 '(aw-scope 'frame)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 10)
 '(custom-buffer-done-kill t)
 '(custom-safe-themes
   '("36b57dcbe8262c52d3123ed30fa34e5ef6b355881674142162a8ca8e26124da9" "6b912e025527ffae0feb76217f1a3e494b0699e5219ab59ea4b3a36c319cea17" "8746b94181ba961ebd07c7397339d6a7160ee29c75ca1734aa3744274cbe0370" "b5c3c59e2fff6877030996eadaa085a5645cc7597f8876e982eadc923f597aca" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" "7397cc72938446348521d8061d3f2e288165f65a2dbb6366bb666224de2629bb" "9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" "8746b94181ba961ebd07c7397339d6a7160ee29c75ca1734aa3744274cbe0370" "b5c3c59e2fff6877030996eadaa085a5645cc7597f8876e982eadc923f597aca" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" default))
 '(default-input-method "russian-computer")
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
 '(icomplete-show-matches-on-no-input t)
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
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(load-prefer-newer t t)
 '(ls-lisp-dirs-first t)
 '(lsp-fsharp-server-args '("--verbose"))
 '(monky-process-type 'cmdserver)
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(mouse-yank-at-point t)
 '(nsm-settings-file "~/.emacs.d/data/network-security.data")
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("w" "Work TODOs " alltodo ""
      ((org-agenda-files
        '("~/Documents/Notes/Worklog.org" "~/Documents/Notes/CRIO.org"))))))
 '(org-agenda-files
   '("/Users/evgeniysharapov/Documents/Notes/Worklog.org" "/Users/evgeniysharapov/Documents/Notes/CRIO.org" "/Users/evgeniysharapov/Documents/Notes/Notes.org" "/Users/evgeniysharapov/Documents/Notes/Ideas.org" "/Users/evgeniysharapov/Documents/Notes/Inbox.org" "/Users/evgeniysharapov/Documents/Notes/journal/20230724_W30.org"))
 '(org-capture-templates
   '(("w" "Work" entry
      (file+olp+datetree "~/Documents/Notes/Worklog.org")
      "* %^{Description}  %(org-set-tags  \":work:\")\12 %t\12%?" :clock-in t :clock-keep t)
     ("m" "Meeting" entry
      (file+olp+datetree "~/Documents/Notes/Worklog.org")
      "* %^{Description}  %(org-set-tags  \":meeting:\")\12 %t\12%?" :clock-in t :clock-keep t)
     ("t" "CRIO Task" entry
      (file+headline "~/Documents/Notes/Inbox.org" "Tasks")
      "* TODO %^{Description}  %(org-set-tags  \":crio:\")\12 %t\12%?")
     ("c" "CRIO Note" entry
      (file "~/Documents/Notes/CRIO.org")
      "* %^{Description}\12%?")
     ("a" "Add Task" entry
      (file+headline "~/Documents/Notes/Inbox.org" "Tasks")
      "* TODO %?\12:PROPERTIES:\12:ID:       %(ffe-uuid)\12:CREATED:  %U\12:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Documents/Notes/Notes.org")
      "* NOTE %? %^G\12:PROPERTIES:\12:ID:       %(ffe-uuid)\12:CREATED:  %U\12:END:" :prepend t)
     ("l" "Link" entry
      (file+headline "~/Documents/Notes/Inbox.org" "Links")
      "* [[%:link][%:description]]\12:PROPERTIES:\12:ID:       %(ffe-uuid)\12:CREATED:  %U\12:URL:      %:link\12:END:\12\12#+BEGIN_QUOTE\12%:initial\12#+END_QUOTE\12\12%?\12")))
 '(org-confirm-babel-evaluate nil)
 '(org-ellipsis " ▼")
 '(org-export-backends '(ascii html icalendar latex md odt confluence))
 '(org-hide-leading-stars t)
 '(org-image-actual-width 600)
 '(org-link-abbrev-alist
   '(("jira" . "https://clinical-research-io.atlassian.net/browse/%s")
     ("opeb" . "https://tracking.ainq.com/browse/OPEB-%s")
     ("edc" . "https://clinical-research-io.atlassian.net/browse/EDC-%s")))
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-modules
   '(org-id ol-info org-mouse org-protocol org-tempo ol-eshell org-eval ol-git-link org-interactive-query org-toc org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-rmail org-tempo org-w3m))
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path "~/.bin/plantuml.jar")
 '(org-refile-targets
   '((org-agenda-files :tag . "")
     (org-agenda-files :maxlevel . 5)
     ("~/Dropbox/Notes/Orgzly/Christianity.org" :maxlevel . 5)) t)
 '(org-refile-use-outline-path 'file)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(outshine-fontify-whole-heading-line t)
 '(outshine-org-style-global-cycling-at-bob-p t)
 '(outshine-use-speed-commands t)
 '(package-selected-packages
   '(polymode 4clojure yasnippet-snippets yaml-mode which-key wgrep-ag visual-regexp visual-fill-column visual-fill utop use-package-ensure-system-package unfill undo-tree toml-mode string-inflection restclient rainbow-mode racer quelpa-use-package pyvenv projectile powershell plantuml-mode pdf-tools paredit outshine org-journal nginx-mode monky modus-themes merlin magit lua-mode lsp-ui lsp-pyright ledger-mode langtool just-mode json-rpc json-navigator json-mode js2-mode ivy-rich imenu-list imenu-anywhere idle-highlight-mode hide-lines helpful groovy-mode go-guru go-eldoc gcmh fsharp-mode flycheck-rust flycheck-pos-tip flycheck-ledger find-file-in-project elisp-slime-nav dockerfile-mode docker diminish dap-mode csv-mode csharp-mode crosshairs counsel company-statistics company-go company-glsl company-c-headers color-moccur cider cargo c-eldoc browse-kill-ring auctex ag adaptive-wrap ace-jump-mode))
 '(projectile-indexing-method 'alien)
 '(projectile-sort-order 'recentf)
 '(python-indent-guess-indent-offset nil)
 '(python-shell-prompt-block-regexp "\\s-+\\.\\.\\. ")
 '(python-shell-prompt-detect-failure-warning nil)
 '(regexp-search-ring-max 100)
 '(safe-local-variable-values
   '((ledger-accounts-file . "accounts.ledger")
     (ledger-accounts-file . "meta.ledger")
     (eval outline-minor-mode t)
     (markdown-asymmetric-header . t)
     (mocha-project-test-directory . "test/server\"")
     (mocha-options . "--reporter spec --recursive --compilers js:babel-core/register")
     (mocha-project-test-directory . "test/server")
     (mocha-which-node)
     (js2-basic-offset . 2)
     (markdown-command . "pandoc -s --toc -S -c assets/style.css -B assets/before.html -A assets/after.html -f markdown -t html")))
 '(savehist-additional-variables
   '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
 '(savehist-autosave-interval 60)
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
 '(warning-suppress-log-types '((modus-themes) (modus-themes)))
 '(warning-suppress-types '((modus-themes) (comp)))
 '(which-function-mode t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Iosevka"))))
 '(fsharp-ui-operator-face ((t (:inherit font-lock-builtin-face)))))

