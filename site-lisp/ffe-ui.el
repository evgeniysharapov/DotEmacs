(require 'cl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   User Experience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-minor-mode 'menu-bar-mode nil)
(custom-set-minor-mode 'tool-bar-mode nil)
(custom-set-minor-mode 'scroll-bar-mode nil)

(when (display-graphic-p)
  (custom-set-minor-mode 'mouse-wheel-mode t)
  (custom-set-minor-mode 'blink-cursor-mode nil))

;; Default typeface
(set-frame-font "Iosevka Light-12" t)

;;; Good typefaces to consider 
;; (set-frame-font "Cousine-11" t)
;; (set-frame-font "Iosevka NF-12:light" t)
;; (set-frame-font "Meslo LG S DZ" t)
;; (set-frame-font "Knack NF" t)
;; (set-frame-font "MesloLGSDZ NF" t)
;; (set-frame-font "MesloLGMDZ NF" t)
;; (set-frame-font "CamingoCode" t)
;; (set-frame-font "InputMonoCondensed Light:light" t)
;; (set-frame-font "InputSerifCondensed Light:light" t)
;; (set-frame-font "InputSansCondensed Light:light" t)
;; (set-frame-font "InputMonoNarrow Light:light" t)
;; (set-frame-font "InputSerifNarrow Light:light" t)
;; (set-frame-font "InputSansNarrow Light:light" t)
;; (set-frame-font "InputSerif" t)
;; (set-frame-font "InputSans" t)
;; (set-frame-font "Fira Code" t)
;; (set-frame-font "Fira Mono for Powerline" t)
;; (set-frame-font "FantasqueSansMono NF-11" t)
;; (set-frame-font "Liberation Mono-11" t)
;; (set-frame-font "Consolas-11" t)
;; (set-frame-font "LiterationMonoPowerline NF" t)
;; (set-frame-font "InconsolataForPowerline NF" t)

(defconst ffe-font-families '(
			      "Anonymous Pro"
			      "Bitstream"
			      "CamingoCode"
			      "Code New Roman"
			      "Consolas"
			      "Cousine"
			      "DejaVu"
			      "Envy Code R"
			      "Fantasque"
			      "Fira"
			      "Hack"
			      "Inconsolata"
			      "Input"
			      "Iosevka"
			      "Knack"
			      "Liberation Mono"
			      "Literation"
			      "Lucida Console"
			      "M+"
			      "Menlo"
			      "Meslo"
			      "Monaco"
			      "Mono"
			      "Monoid"
			      "NotCourierSans"
			      "Pragmata"
			      "Roboto Mono"
			      "Source Code"
			      "mononoki"
			      ))
(defconst --filter-fonts (regexp-opt ffe-font-families) "Regular expression that covers possible usability")

(defun ffe-select-typeface ()
  "Choose typefaces for the frame"
  (interactive)
  (set-frame-font (ido-completing-read+ "Choose font:"
					(cl-remove-if-not
					 (lambda (e) (string-match-p --filter-fonts e))
					 (font-family-list)))
		  t))

(bind-key "f" #'ffe-select-typeface ctl-x-t-map)

(bind-key "w" #'whitespace-mode ctl-x-t-map)

(provide 'ffe-ui)
