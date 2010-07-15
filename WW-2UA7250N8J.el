;;
;; Disney Specific Emacs Configuration.
;; We know that it's a Windows Machine 
;; Evgeniy Sharapov, 2010 
;;
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
; maximize Emacs frame
(global-set-key [(control f11)]
                (make-interactive w32-send-sys-command #xf030 nil))
  ; restore original size of the Emacs frame
(global-set-key [(control shift f11)]
                (make-interactive w32-send-sys-command #xf120 nil))
(set-face-attribute 'default nil :family "Consolas" :height 100)

;;
;;  Setting SSH connection
;;
(require 'tramp)
(setq tramp-default-method "plink")
;;
;; Define new faces for shell
;;
;; (defface sh-special-vars
;;   '((t (:weight bold :inherit font-lock-variable-name-face)))
;;   "Shell special variables"
;;   :group 'faces)

;; (defface sh-var-content
;;   '((t (:weight bold :inherit font-lock-variable-name-face)))
;;   "Shell special variables"
;;   :group 'faces)


;; ;; XXX: redefining face defined in sh-script.el
;; (defface sh-quoted-exec
;;   '((((class color) (background light))
;;      (:background "gray85")))
;;   "Shell command substitution"
;;   :group 'faces)

;; (defun sen-sh-syntax ()
;;   (font-lock-add-keywords 'sh-mode
;;        '(("\\<\\(awk\\|alias\\|bg\\|bind\\|break\\|builtin\\|cd\\|command\\|continue\\|cut\\|declare\\|typeset\\|dirs\\|disown\\|echo\\|enable\\|eval\\|exec\\|exit\\|export\\|fc\\|fg\\|getopts\\|hash\\|help\\|history\\|jobs\\|kill\\|let\\|local\\|logout\\|popd\\|printf\\|pushd\\|pwd\\|read\\|readonly\\|return\\|sed\\|set\\|shift\\|shopt\\|suspend\\|test\\|times\\|trap\\|type\\|type\\|ulimit\\|umask\\|unalias\\|uniq\\|unset\\|wait\\)\\>" . 'font-lock-builtin-face)
;;          ("${?[#@*$!?0-9]\\|PWD\\|UID\\OPTARG\\|OPTIND\\|PPID\\|HOSSTNAME}?" . 'sh-special-vars)
;;          ("${?[a-zA-Z_][0-9a-zA-Z_]*}?" . sh-var-content)
;;          )
;;        ))
(defface sh-special-variables-face
  '((t (:weight bold :inherit font-lock-variable-name-face)))
  "Shell special variables"
  :group 'faces)
(font-lock-add-keywords 'shell-script-mode
                        '(
                          ("${?\\([#@*$!?0-9]\\|PWD\\|UID\\OPTARG\\|OPTIND\\|PPID\\|HOSSTNAME\\)}?" . 'sh-special-variables-face)
                          ))


;;
;; Addign Yasippets 
;;
(if (and (file-directory-p "~/.emacs.d/snippets")
         (fboundp 'yas/load-directory))
    (yas/load-directory "~/.emacs.d/snippets"))
