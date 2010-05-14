;;
;; Evgeniy Sharapov 
;; Light Color Theme 
;; 2010
;;
;; MIT License Copyright (c) 2010 Evgeniy Sharapov
;; <evgeniy.sharapov@gmail.com>
;;
(require 'color-theme)

(defvar sen-fg "")
(defvar sen-bg "")

(defun color-theme-esharapov ()
  "This is my light color-theme"
  (interactive)
  (color-theme-install
   '(color-theme-esharapov
     ((foreground-color . "black")
      (background-color . "#f8f8f8")
      (background-mode . light))
     (default ((t nil))))
   ))
