;; init-rust.el --- Initialize Rust Mode -*- lexical-binding: t -*-

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (setq rust-mode-treesitter-derive t)
  (add-hook 'rust-mode-hook
            (lambda () (prettify-symbols-mode)))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)
