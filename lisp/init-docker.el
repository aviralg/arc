;; init-docker.el --- Initialize Docker -*- lexical-binding: t -*-

;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(provide 'init-docker)
