;;  
;;  New Emacs Configuration
;;  Utility functions. Too big and clumsy to put in settings
;;  Evgeniy Sharapov <evgeniy.sharapov@gmail.com>
;;

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro with-library (symbol &rest body) 
  "A typical use is: you want to load a
 library, and then bind some function from that library to a key. But
 you don't want to attempt the binding if the library wasn't present,
 because that will throw an error, or something. This is particularly
 handy when you use the same .emacs on different machines, not all of
 which have the same libraries available.
 Example:
     (with-library tabbar
       (tabbar-mode)
       (global-set-key [(control tab)]       'tabbar-forward)
       (global-set-key [(control shift tab)] 'tabbar-backward)
       (global-set-key [(control shift iso-lefttab)] 'tabbar-backward)
       (global-set-key [(control f10)] 'tabbar-local-mode))

 In this example, if the library tabbar isn't available, Emacs will
 simply put a message in the echo area to that effect, and wonâ_Tt even
 try to call global-set-key." 
  `(condition-case nil 
       (progn 
         (require ',symbol)
         ,@body) 
     (error 
      (message (format "%s is not available." ',symbol))
      nil)))

(defmacro make-interactive (func &rest args) 
  "Returns a symbol of an anonymous interactive function, suitable for binding to keys." 
  `(lambda () 
     (interactive) 
     (,func ,@args)))

(defun pretty-greek () 
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon"
                 "phi" "chi" "psi" "omega"))) 
    (loop for word in greek for code = 97 then (+ 1 code) do  
          (let ((greek-char (make-char 'greek-iso8859-7 code))) 
            (font-lock-add-keywords 
             nil
             `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]") 
                (0 
                 (progn 
                   (decompose-region (match-beginning 2) 
                                     (match-end 2))
                   nil))))) 
            (font-lock-add-keywords 
             nil
             `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]") 
                (0 
                 (progn 
                   (compose-region (match-beginning 2) 
                                   (match-end 2) ,greek-char)
                   nil)))))))))  

(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

(provide 'init-defuns)

