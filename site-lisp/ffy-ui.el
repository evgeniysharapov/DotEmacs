;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			   User Experience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-minor-mode 'menu-bar-mode nil)
(custom-set-minor-mode 'tool-bar-mode nil)
(custom-set-minor-mode 'scroll-bar-mode nil)

(when (display-graphic-p)
  (custom-set-minor-mode 'mouse-wheel-mode t)
  (custom-set-minor-mode 'blink-cursor-mode nil))

;; typefaces
(set-frame-font "Iosevka Light-12" t)
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

(setq ffy-type-faces '("DejaVu LGC Sans Mono"
                       "Consolas"
                       "Source Code Pro"
                       "Monaco"
                       "Menlo"
                       "Meslo LG L DZ"
                       "Meslo LG M DZ"
                       "Meslo LG S DZ"
                       "Bitstream Vera Sans Mono"
                       "Anonymous Pro"
                       "Inconsolata LGC"
                       "Envy Code R"
                       "PragmataPro"
                       "Pragmata TT"
                       "NotCourierSans"
                       "Liberation Mono"
                       "Hack"
                       "Fira Code"
                       "Code New Roman"
                       "Audimat Mono:light"
                       "CamingoCode"
                       "Input"
                       "Iosevka"
                       "mononoki"
                       "Monoid"
                       "Lucida Console"
                       "Roboto Mono Light"
                       "M+ 1m"))

(defun ffy-select-typeface ()
  "Choose typefaces for the frame"
  (interactive)
  (set-frame-font (ido-completing-read+ "Choose font:" ffy-type-faces nil nil) t))

(bind-key "f" #'ffy-select-typeface ctl-x-t-map)


(provide 'ffy-ui)
