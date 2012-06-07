;;
;; Disney Specific Emacs Configuration.
;; We know that it's a Windows Machine 
;; Evgeniy Sharapov, 2010 
;;

;;
;;  Setting SSH connection
;;
(require 'tramp)
(setq tramp-default-method "plink")


;;
;; ORG-Mode Special Tweaks
;;
;; Adding functionality to integrate with Outlook
(require 'org-outlook)
(when (fboundp 'w32-short-file-name)
  (setq org-outlook-location (w32-short-file-name "c:/Program Files/Microsoft Office/OFFICE12/OUTLOOK.exe")))

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



;; Here we started work on ALC log files mode
(defgroup disney-alc-log nil
  "Major mode for viewing log files."
  ;; :link '(emacs-library-link :tag "Source File" "")
  :group 'faces
  :group 'files)

(defun -format-xml (start end indentation)
  "Formats XML using xmlstartlet command line tool"
  (shell-command-on-region start end (format "xml fo -s %s -o" indentation) nil t))

(defun disney-format-xml (start end &optional indent)
  "Formats region as XML"
  (interactive "r")
  (if indent
    (-format-xml start end indent)
    (-format-xml start end 2))
  ;; now go to the mark
  (goto-char (mark)))

(defun disney-format-log ()
  "Formats INC excerpts, getting rid of long line and making SOAP messages look prettier"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":Envelope" nil t)
      (search-backward "<" nil t)
      (setq start (point))
      (re-search-forward ":Envelope>" nil t)
      (disney-format-xml start (point)))))

(defun disney-occur-entries ()
  "Calls occur on regular expression that marks entries in log excerpts from A La Carte"
  (interactive)
  (occur (rx ?> ?> (1+ blank) "SEClient"
            (1+ blank) ?> ?> (1+ blank) (1+ alnum))))

(define-minor-mode disney-alc-log-mode
  "Mode that helps us to work with ALC log files"
  :init-value nil
  :lighter "ALC"
  :keymap '()
  :group  'disney-alc-log
  )

(defun disney-alc-log-mode ()
  "Toggle "
  (interactive)
  ;; Turn on font-lock-mode
  (if (not font-lock-mode)
      (font-lock-mode 1))
  )
