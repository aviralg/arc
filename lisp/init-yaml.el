;; init-yaml.el --- Initialize YAML Mode -*- lexical-binding: t -*-

;;; Code: Set up `yaml-mode'
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.clangd\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode)))

(provide 'init-yaml)

;;; init-yaml.el ends here
