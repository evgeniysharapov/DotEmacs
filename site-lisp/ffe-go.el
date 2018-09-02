;; Go language setup
;;
;; Requires GOPATH and PATH to be setup and include go executables
;;
;; Documentation:
;; Oracle    https://docs.google.com/document/d/1SLk36YRjjMgKqe490mSRzOPYEDe0Y_WQNRv-EiFYUyw/view
;; Guru    https://docs.google.com/document/d/1_Y9xCEMj5S-7rv2ooHpZNH15JgRT5iM742gJkw5LtmQ/edit

;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/lint/golint
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode

;; TODO:
;; package-install flymake-go
;; go get -u github.com/dougm/goflymake
;; other packages to look at
;;  go-add-tags
;;  go-fill-struct
;;  go-gen-test
;;  go-impl
;;  go-projectile
;;  go-rename
;;  go-tag
;;  golint

(use-package go-mode
  :ensure t
  :init (progn
          (use-package go-eldoc
            :ensure t
            :init (add-hook 'go-mode-hook #'go-eldoc-setup))
          (use-package go-guru
            :ensure t
            :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))
          (use-package company-go
            :ensure t
            :init (add-hook 'go-mode-hook (lambda () (ffe-add-company-backends 'company-go))))

          (defun go-run-buffer ()
            "This will run buffer on the Go"
            (interactive)
            (compile (concat "go run " (buffer-file-name))))

          (add-hook 'go-mode-hook (lambda ()
                                    ;; customize  compile command for go-mode
                                    (set (make-local-variable 'compile-command)
                                         "go build")
                                    ;; make before-save-hook local for go-mode buffer
                                    (add-hook 'before-save-hook 'gofmt nil t)))
          (bind-keys :map go-mode-map
                     :prefix "C-c C-d"
                     :prefix-map go-mode-doc-map
                     ("h"   . godoc)
                     ("d"   . godef-describe)
                     ("C-d" . godoc-at-point)))
  
  :config (progn
            (setq gofmt-command "goimports"))
  
  :bind (("C-c C-c" . go-run-buffer)))

(provide 'ffe-go)
