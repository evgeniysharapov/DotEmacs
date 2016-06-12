;;
;; Infusion laptop Settings File
;; We know that it's a Windows Machine
;; Evgeniy Sharapov, 2013
;;

;;; Windows specific stuff
(when (equal window-system 'w32)
    (setq
       w32-pass-lwindow-to-system nil
       w32-lwindow-modifier 'super
       w32-pass-rwindow-to-system nil
       w32-rwindow-modifier 'super
       w32-pass-apps-to-system nil
       w32-apps-modifier 'hyper
       w32-pass-alt-to-system nil
       w32-scroll-lock-modifier nil))

;;; Trick from http://www.emacswiki.org/emacs/InteractiveSpell
(setenv "DICTIONARY" "en_US")
;;; manage PATH for running Git from MsysGit (Cygwin doesn't work
;;; properly with Magit). Important we should sneak MsysGit path in
;;; front
;;; Turns out that msysGit contains older gnu tools (e.g. Grep version
;;; 2.4). Add more recent path upfront
(let ((paths '("C:/App/EZWin/bin" "C:/App/gnuw32/bin" "C:/PF/Git/bin" "C:/PF/Git/usr/bin" "C:/PF/Git/cmd")))
  (setenv "PATH" (mapconcat  'identity (append paths (list (getenv "PATH"))) ";"))
  (mapc (apply-partially 'add-to-list 'exec-path) paths))

;;; C/C++ Headers Locations
(setq ffy-c-headers-locations '("C:/PF86/LLVM/lib/clang/3.6.2/include"
                                "C:/PF86/LLVM/i686-w64-mingw32/include"
                                "C:/PF86/LLVM/i686-w64-mingw32/include/c++"
                                "C:/PF86/LLVM/i686-w64-mingw32/include/c++/i686-w64-mingw32"
                                "C:/PF86/LLVM/include/"))

;;; Org-mode hackery
(setq org-clock-clocktable-default-properties
      (list :maxlevel 4
            :scope 'file
            :step 'day
            :block 'week
            :narrow '80!
            :link t
            :properties '("OpenAir" "Support")
            :inherit-props t))

;;; Font properties on my work Windows machine
;; (set-face-attribute 'default nil :family "Consolas" :height 100)
;; (set-default-font "DejaVu LGC Sans Mono-10")
;; (set-default-font  "Consolas-10")
;; (set-default-font "Source Code Pro-10")
;; (set-default-font  "Monaco-10")
(set-frame-font "Meslo LG L DZ-10")
;; (set-default-font "Bitstream Vera Sans Mono-10")
;; (set-default-font "Anonymous Pro-10")
;; (set-default-font "Inconsolata LGC-10")
;; (set-default-font "Envy Code R-10")

;;; loading up color theme
;(load-theme 'tango-plus t) ; need to add it to the 'custom-theme-load-path
(load "tango-plus-theme.el")
