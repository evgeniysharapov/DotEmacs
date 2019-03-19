;;; package --- summary
;;; Configuration for Precision Laptop from Ai
;;; Commentary:
;;; Code:

(when (string-equal system-type "gnu/linux")
  ;; Emacs is running from WSL
  (add-to-list 'default-frame-alist '(font . "Iosevka-13"))
  (set-face-attribute 'default (selected-frame) :height 135))

(when (string-equal system-type "windows-nt")
  ;; Emacs is running from Windows
  (add-to-list 'default-frame-alist '(font . "Iosevka-12"))
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super    ; Left Windows key
        w32-pass-apps-to-system nil
        w32-apps-modifier 'hyper)      ; Menu/App key
  )

;;; P3530-20190221.el ends here



