;;; modules/shell.el --- Shell -*- lexical-binding: t; -*-

;; Eshell with consult-history integration (C-r for searchable history).
(use-package eshell
  :ensure nil
  :config
  (setq eshell-scroll-to-bottom-on-input 'this
        eshell-destroy-buffer-when-process-dies t)
  ;; Bind C-r to consult-history in eshell for fuzzy history search
  (defun my--eshell-consult-history ()
    (keymap-set eshell-mode-map "C-r" #'consult-history))
  (add-hook 'eshell-mode-hook #'my--eshell-consult-history))

;;; modules/shell.el ends here
