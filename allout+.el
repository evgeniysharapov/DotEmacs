;;; allout+.el - Some additional features for the AllOut mode.

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Copyright (C) 2013 Evgeniy Sharapov
;; Filename      : allout+.el
;; Description   : .
;; Author        : Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;; Created       : 
;; Version       : 0.1
;; Keywords      : help
;; X-URL         : https://evgeniysharapov
;; Last-Updated  : Fri Dec 28 09:52:24 2012 (-0800)
;; Update #      : 1499
;; URL           : http://www.emacswiki.org/help-fns+.el
;; Doc URL       : http://emacswiki.org/HelpPlus
;; Keywords      : help, faces, characters, packages, description
;; Compatibility : GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that are required by this library:
;;  `s', `dash' 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:

(autoload 's-repeat "s")

;; (defun ffy-allout-defface (level fg)
;;   "Helper function that defines foreground color for the Allout header"
;;   (let ((face-name (intern (concat "ffy-allout-header-" (int-to-string level)))))
;;      (make-face face-name)
;;      (copy-face 'font-lock-comment-face face-name)
;;      (face-foreground 'outline-1)
;;      (set-face-foreground face-name fg)
;;      face-name))

;; (defun ffy-allout-font-lock-headers ()
;;   "Adds fontification of Allout headers in Lisp files. Uses Outline faces as faces for AllOut levels"
;;   (when (require 'outline nil t)
;;     (font-lock-add-buffer-keywords
;;      (let ((lvl 0))
;;        (mapcar (lambda (f)
;;                  (progn
;;                    (setq lvl (1+ lvl))
;;                    (list
;;                     ;; regexp for Allout header
;;                     (if (> lvl 1)
;;                         (concat "^\\(;;;_[" (s-repeat (- lvl 1) " ")  "][\\.,][ ]*\\(.+\\)\\)" )
;;                       (concat "^\\(;;;_" "[\\.,][ ]*\\(.+\\)\\)" ))
;;                     2                         ; matched group
;;                     (ffy-allout-defface lvl (face-foreground f nil t)) ; face
;;                     ;'prepend
;;                     t
;;                     )))
;;                outline-font-lock-faces)))))

(provide 'allout+)
