;;; Configuration for the Rust programming language
;;;
;;; This setup expects that one has installed rust system using `rustup-init`,
;;; there's a `cargo` tool and we installed `racer` and `rustfmt` crates. Check out corresponding documentation
;;; on how to do that. More over there's a RUST_SRC_PATH environment  variable pointing to rust sources. 
;;; 
(use-package rust-mode
  :ensure t
  :defer t)

(use-package cargo
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; Auto completion for rust
(use-package racer
  :ensure t
  :defer t
  :after rust-mode
  :init (add-hook 'rust-mode-hook #'racer-mode)   
  :config (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

;; Major mode for .toml Cargo files
(use-package toml-mode
  :ensure t
  :defer t)

(provide 'ffe-rust)
