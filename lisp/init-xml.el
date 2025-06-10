;; init-xml.el --- Initialize XML -*- lexical-binding: t -*-

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(provide 'init-xml)
