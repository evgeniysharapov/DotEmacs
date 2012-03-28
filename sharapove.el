;;;
;;; Few settings on Prognoz computer
;;;

;;;
;;; For some reason C-) and C-( doesn't work on it. I can't even get a
;;; C-h k on them. So for paredit we will use C-right and C-left
;;;

(eval-after-load "paredit"
  '(progn
    (define-key paredit-mode-map [(control right)] 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map [(control left)] 'paredit-backward-slurp-sexp)))
