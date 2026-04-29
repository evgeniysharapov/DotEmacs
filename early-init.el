;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; Loaded before init.el, package system, and GUI initialization.
;; Used for performance optimization and preventing visual artifacts.

;;; Code:

;; Defer garbage collection during startup for significant speedup
;; gcmh package will manage GC during normal operation
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore reasonable GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 104857600   ; 100MB (from custom.el)
                  gc-cons-percentage 0.1)))

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-speed 2))

;; Reduce startup overhead by temporarily disabling file handlers
(defvar ffe--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist ffe--file-name-handler-alist)))

;; Disable UI elements before frame creation (prevents flashing)
;; Moved from init.el lines 303-305
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)))

;; Package system configuration (runs before package-initialize)
;; Moved from init.el line 78
(setq package-enable-at-startup nil
      package-quickstart t)  ; Faster package loading

;;; early-init.el ends here
