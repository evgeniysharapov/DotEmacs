;;; colorizer.el --- Helps to Work with colors
;; 
;; Filename: colorizer.el
;; Description: This is a library that works with colors. Quite
;; helpful when you need to create color scheme for color-theme for
;; example. 
;; Author: Evgeniy N. Sharapov 
;; Maintainer: 
;; Created:  
;; Version: 
;; URL: http://www.feastforeyes.com
;; Keywords: color, color-theme, colors, palettes, syntax
;; highlighting, 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun colorp (object)
  "Return t if given object is a triplet"
  (and (sequencep object)
            (= (length object) 3)))
(defun rgbp (color)
  "Returns true if every component of the color triplet is in 0 to 255
range"
  (every (lambda (c) (and (<= c 255) (>= c 0))) color))

(defun RGB-to-rgb (color)
  (mapcar (lambda (c) (lsh c -8)) color))

(defun rgb-to-hex (color)
  "Returns string - a HEX representation of the RGB color.  
Color can be in RGB, i.e. triplet where each component is between
0 and 255; or Emacs RGB color, i.e. triplet where each component
ranges from 0 to 65280 or 65535, depending on the system; white
is \(65280 65280 65280\) or \(65535 65535 65535\).
So \(color-to-hex '\(255 255 255 \)\) returns #fffff and
\(color-to-hex '\(65535 65535 65535\)\) return #ffffff"
  (apply 'format "#%02x%02x%02x"
         (if (rgbp color) color
           (RGB-to-rgb color))))

(defun hex-to-rgb (hex)
  "Converts given \"#xxxxxxx\" or \"xxxxxx\" string into an RGB triplet"
(let ((hex (cond ((string-match "#\[0-9a-fA-F\]+" hex) (substring hex 1))
                 ((string-match "\[0-9a-fA-F\]+" hex) hex))))
  (mapcar (lambda (s) (string-to-number s 16))
          (mapcar (lambda (n) (substring hex n (+ n 2))) '(0 2 4)))))

;;
;; Give it more meaningful names to RGB components 
;;
(defsubst red (color)
  (car color))
(defsubst green (color)
  (cadr color))
(defsubst blue (color)
  (caddr color))
;;  HSL model
(defsubst hue (color)
  (car color))
(defsubst saturation (color)
  (cadr color))
(defsubst light (color)
  (caddr color))

(defmacro >< (arg lbound rbound)
  "Returns true if ARG is greater than LBOUND and less than
RBOUND "
  `(and (> ,arg ,lbound) (< ,arg ,rbound)))

(defmacro =>< (arg lbound rbound)
  "Returns true if ARG is greater than or equal to LBOUND and
less than RBOUND "
  `(or (>< ,arg ,lbound ,rbound) (= ,arg ,lbound)))

(defmacro ><= (arg lbound rbound)
  "Returns true if ARG is greater than LBOUND and less than or
equal to RBOUND"
  `(or (>< ,arg ,lbound ,rbound) (= ,arg ,rbound)))

(defmacro =><= (arg lbound rbound)
  "Returns true if ARG is greater than or equal to LBOUND and
  less than or equal to RBOUND"
  `(or (>< ,arg ,lbound ,rbound) (= ,arg ,lbound) (= ,arg ,rbound)))

(defun hue360 (hue)
  "Converts given HUE into the [0,360\) range."
  (cond
   ((=><= hue 0 360) hue)
   ((< hue 0) (hue360 (+ hue 360)))
   ((> hue 360) (hue360 (- hue 360)))))

(defun rgb-to-hsl (color)
  "Converts RGB into HSL color triplet.  
More information:
http://en.wikipedia.org/wiki/HSL_and_HSV and
http://en.wikipedia.org/wiki/RGB_color_model"
  (let* ((r (* 0.2 (/ (red color) 51.0)))
         (g (* 0.2 (/ (green color) 51.0)))
         (b (* 0.2 (/ (blue color) 51.0)))
         (mx (max r g b))
         (mn (min r g b))
         (dif (- mx mn))
         (sum (+ mx mn))
         (light (/ (+ mx mn) 2.0))
         (saturation (if (= mx mn) 0.0
                       (if (< light 0.5) (/ dif sum)
                         (/ dif (- 2 sum)))))
         (hue (round
               (* 60 (cond
                      ((= mx r) (/ (- g b) dif))
                      ((= mx g) (+ 2 (/ (- b r) dif)))
                      ((= mx b) (+ 4 (/ (- r g) dif))))))))
    (list (hue360 hue)
          (round (* saturation 100.0))
          (round (* light 100.0)))))

(defun hsl-to-rgb (color)
  "Converts HSL color triplet into RGB. 
More information: http://en.wikipedia.org/wiki/HSL_and_HSV"
  (let* ((H (hue color))
         (S (/ (saturation color) 100.0))
         (L (/ (light color) 100.0)))
    (if (= 0.0 S)
        (make-list 3 (round (* 255 L)))
      (let* ((C (if (<= L 0.5) (* 2 L S)
                      (- (* 2 S) (* 2 L S))))
             (Hprime (/ H 60.0))
             (X (* C (- 1 (abs (- (mod Hprime 2) 1)))))
             (m (- L (* C 0.5))))
        (mapcar (lambda (c) (round (* c 255)))
                (cond
                 ((=>< Hprime 0 1) (list (+ C m) (+ X m) m))
                 ((=>< Hprime 1 2) (list (+ X m) (+ C m) m))
                 ((=>< Hprime 2 3) (list  m (+ C m) (+ X m)))
                 ((=>< Hprime 3 4) (list  m (+ X m) (+ C m)))
                 ((=>< Hprime 4 5) (list (+ X m) m (+ C m)))
                 ((=>< Hprime 5 6) (list (+ C m) m (+ X m)))
                 (t (list m m m))))))))

(defun colorizer-lighten (color &optional step)
  "Takes given RGB color and returns one that is more bright"
  (let* ((lightness-change (if (null step) 5 step))
         (hsl-color (rgb-to-hsl color))
         (new-light (+ (light hsl-color) lightness-change)))
    (list (hue hsl-color) (saturation hsl-color)
          (if (> new-light 100) 100 new-light))))

(defun colorizer-darken (color &optional step)
  "Takes given RGB color and returns one that is more dark "
  (let* ((darkness-change (if (null step) 5 step))
         (hsl-color (rgb-to-hsl color))
         (new-light (- (light hsl-color) darkness-change)))
    (list (hue hsl-color) (saturation hsl-color)
          (if (< new-light 0) 0 new-light))))

(defun colorizer-generate-palette (color))

(provide 'colorizer)
