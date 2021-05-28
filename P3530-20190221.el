;;; package --- summary
;;; Configuration for Precision Laptop from Ai
;;; Commentary:
;;; Code:

(defun set-font-if-available (font-name)
  "Checks if font FONT-NAME is available and sets it as a default for a frame."
  (when (find-font (font-spec :name font-name))
    (add-to-list 'default-frame-alist  (cons 'font font-name))))

(defun try-fonts (font-name-list)
  "Goes over list FONT-NAME-LIST and tries to set a font name from the list as the default.

 Stops once it either exhausted the list or successfully installed the font."
  (cl-loop for font-name in font-name-list
           until (set-font-if-available font-name)))

(when (string-equal system-type "gnu/linux")
  ;; Emacs is running from WSL

  ;; for running 'emacs --daemon' we store .emacs.desktop in a different spot
  (add-hook 'desktop-save-hook
            (lambda ()
              (customize-set-variable 'desktop-base-file-name ".emacs.desktop.wsl")))

  (add-to-list 'default-frame-alist '(font . "Iosevka"))
  (set-face-attribute 'default (selected-frame) :height 135))

(when (string-equal system-type "windows-nt")
  ;; Emacs is running from Windows
  (try-fonts '("Iosevka"
               "Iosevka SS09"
               "Consolas"
               "Cascadia Code PL"))
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super    ; Left Windows key
        w32-pass-apps-to-system nil
        w32-apps-modifier 'hyper)      ; Menu/App key
  ;; Put my tools bin directory in front of the path
  
  (setenv "PATH" (concat "C:\\Users\\esharapov\\Tools\\Bin;" (getenv "PATH")))

  (setq ffip-use-rust-fd t)
)
;;; P3530-20190221.el ends here



