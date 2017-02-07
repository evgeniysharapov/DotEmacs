;;; C/C++ Configuration mostly for one off c/c++ programs
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode)))

;;; C/C++ Headers Locations. This is system specific
(defconst *ffe-c-headers-dirs* '("C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++\\x86_64-w64-mingw32"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include\\c++\\backward"
				 "C:\\PF\\LLVM\\lib\\clang\\3.7.0\\include"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include"
				 "C:\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\6.2.0\\include-fixed"
				 "C:\\mingw64\\x86_64-w64-mingw32\\include"
				 "C:\\mingw64\\include"))

(use-package company-c-headers
  :defer t
  :commands company-c-headers
  :ensure t
  :config
  (dolist (dir (if (boundp '*ffe-c-headers-dirs*)
		   *ffe-c-headers-dirs*
		 ()))
    (add-to-list 'company-c-headers-path-system dir)))

(use-package c-eldoc
  :defer t
  :commands c-turn-on-eldoc-mode
  :ensure t
  :config (setq c-eldoc-includes
                (mapconcat #'identity
                           ;; on Windows `pkg-config` .... leads to an
                           ;; error
                           (cons ;c-eldoc-includes
                                 "-I. -I.."
                                 (mapcar (apply-partially #'concat "-I")
                                         *ffe-c-headers-dirs*))
                           " ")
                c-eldoc-cpp-command "cpp"))

(defun ffe-c-mode-hook ()
  "This is settings for the C/C++ mode

Due to a bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759 add it to a c-mode-common-hook"
  (when (memq major-mode '(c-mode c++-mode))
    (electric-pair-mode +1)
    (electric-indent-local-mode +1)
    (c-toggle-hungry-state +1)
    (c-set-style "gnu")
    (setq c-basic-offset 4)
    (c-turn-on-eldoc-mode)
    (set (make-local-variable 'compile-command)
         (let ((f (file-name-nondirectory (buffer-file-name))))
           (case major-mode
             ('c-mode (format "gcc -g -O2 -std=gnu99 -static -lm %s" f))
             ('c++-mode (format "g++ -g -O2 -static -std=gnu++11 %s" f))
             (t compile-command))))

    (ffe-add-company-backends 'company-c-headers 'company-semantic 'company-clang 'company-xcode)))

(provide 'ffe-c)