;;; Feast For Eyes theme --- Emacs theme that is tailored to work in GUI and Terminal
;;
;;
;; Author: Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;; Version: 0.0.1
;; Keywords: themes light
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;; Licenese:
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;; Code:
;;
(require 'color)

(deftheme feast-for-eyes
  "Feast For Eyes theme --- Emacs theme that is tailored to work in GUI and Terminal")

(defmacro with-alist (alist &rest body)
  "Create a `let' with ALIST as a `let' variables and wraps it around the BODY."
  `(let (,@(mapcar (lambda (cell) (list (car cell) (cdr cell)))  (eval alist)))
     ,@body))

;;
;; Helper functions from https://oremacs.com/2015/04/28/blending-faces/
(defun colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

;;
(let ((gui '((class color) (type graphic)))
      (term '((class color) (type tty))))
  (with-colors-alist
   '(
     (ffe-light . "ivory2")
     (ffe-dark  . "")
     (ffe-blue . "#424dde")
     (ffe-yellow . "#dedb42")
     (ffe-magenta . "#d442d4")
     (ffe-green . "#4be849")
     (ffe-red . "#de4247"))
    (custom-theme-set-faces
     'feast-for-eyes
     ;; '(bold ((t (:bold t))))
     ;; '(bold-italic ((t (:italic t :bold t))))
     ;; '(info-node ((t (:italic t :bold t))))
     ;; '(info-xref ((t (:bold t))))
     ;; '(italic ((t (:italic t))))
     ;; '(underline ((t (:underline t))))
     ;; '(fringe ((t (:background "#f7f7f7"))))
     ;; '(mode-line ((t (:background "#e8e8e8" :foreground "black" :box (:line-width -1 :color "#a5a5a5")))))
     ;; '(mode-line-highlight ((t (:box (:line-width 2 :color "#9599B0")))))
     ;; '(mode-line-inactive ((t (:inherit mode-line :background "#f5f5f5" :foreground "grey20" :box (:line-width -1 :color "#a5a5a5") :weight light))))
     ;; '(show-paren-match ((t (:background "#AAAAAA"))))
     ;; '(show-paren-mismatch ((t (:foreground "#F9F2CE" :background "#F93232"))))
     `(hl-line ((,gui (:background ,ffe-green))
                (,term (:background ,ffe-yellow))))
     ;; '(highlight ((t (:background "#EEE00A"))))
     ;; '(isearch ((t (:background "#EEE00A"))))
     ;; '(lazy-highlight ((t (:background "#FBE9AD"))))
     ;; (region ((t (:background "ns_selection_color"))))
     ;; '(secondary-selection ((t (:background "#FBE9AD"))))
     ;; '(shadow ((t (:foreground "grey50" :background "#EEEEEE"))))
     ;; '(font-lock-builtin-face ((t (:foreground "#626FC9"))))
     ;; '(font-lock-comment-face ((t (:foreground "#7F7F7F"))))
     ;; '(font-lock-constant-face ((t (:foreground "#7653C1" :background "#F3F2FF"))))
     ;; '(font-lock-function-name-face ((t (:foreground "#4E279A"))))
     ;; '(font-lock-keyword-face ((t (:foreground "#6700B9"))))
     ;; '(font-lock-preprocessor-face ((t (:foreground "#434343"))))
     ;; '(font-lock-string-face ((t (:foreground "#BC670F" :background "#FDFBF5"))))
     ;; '(font-lock-type-face ((t (:foreground "#699D36"))))
     ;; '(font-lock-variable-name-face ((t (:foreground "#7B8C4D"))))
     ;; '(font-lock-warning-face ((t (:foreground "#F93232"))))
     ;; '(diff-file-header ((t (:bold t :inherit diff-header))))
     ;; '(diff-header ((t (:background "#DDDDFF" :foreground "grey20"))))
     ;; '(diff-added ((t (:background "#DDFFDD"))))
     ;; '(diff-removed ((t (:background "#FFDDDD"))))
     ;; '(diff-changed ((t (:background "#FFFFDD"))))
     ;; '(diff-refine-changed ((t (:background "#DDDDFF"))))
     ;; '(default ((t (:backround "grey99"))))))
     )))

(provide-theme 'feast-for-eyes)
;;; feast-for-eyes-theme.el ends here

