;;; Visibility of the text in a window
(defvar ctl-z-map)

;;; C-z /          hides lines matching regexp
;;; C-u C-z /      hides lines not matching regexp
;;; C-u C-u C-z /  unhides everything
(use-package hide-lines
  :defer t
  :bind (:map ctl-z-map
	      ("/" . hide-lines)))

(provide 'ffe-visibility)
