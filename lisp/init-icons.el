;; init-icons.el --- Initialize icons -*- lexical-binding: t -*-

;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t)

;; https://github.com/LuigiPiucco/nerd-icons-corfu
;; INFO: FIX THIS
(use-package nerd-icons-corfu
  :ensure t
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package compile-multi-nerd-icons
  :ensure t
  :after (compile-multi nerd-icons-completion)
  :demand t)

(provide 'init-icons)
