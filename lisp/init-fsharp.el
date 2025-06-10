;; init-fsharp.el --- Initialize F# -*- lexical-binding: t -*-

(use-package fsharp-mode
  :ensure t)

(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :config
  (add-hook 'fsharp-mode-hook #'eglot-ensure))

(provide 'init-fsharp)
