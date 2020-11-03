;;; package --- summary
;;; Configuration for Precision Laptop from Ai
;;; Commentary:
;;; Code:

(when (string-equal system-type "gnu/linux")
  ;; Emacs is running from WSL

  ;; for running 'emacs --daemon' we store .emacs.desktop in a different spot
  (add-hook 'desktop-save-hook
            (lambda ()
              (customize-set-variable 'desktop-base-file-name ".emacs.desktop.wsl")))
  
  ;; we would rarely run it in Graphics mode but nonetheless
  (add-to-list 'default-frame-alist '(font . "Iosevka"))
  ;; (set-face-attribute 'default (selected-frame) :height 135)
  )

(when (string-equal system-type "windows-nt")
  ;; Emacs is running from Windows
  (add-to-list 'default-frame-alist '(font . "Iosevka-12"))
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super    ; Left Windows key
        w32-pass-apps-to-system nil
        w32-apps-modifier 'hyper)      ; Menu/App key
  ;; Put my tools bin directory in front of the path
  
  (setenv "PATH" (concat "C:\\Users\\esharapov\\Tools\\Bin;" (getenv "PATH")))

  (setq ffip-use-rust-fd t)
)
;;; P3530-20190221.el ends here



