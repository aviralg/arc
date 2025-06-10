;; init-r.el --- Initialize R -*- lexical-binding: t -*-

;; TODO - study settings
;; https://ess.r-project.org/Manual/ess.html
(use-package ess
  :ensure t
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake t
        ess-nuke-trailing-whitespace-p t
        ess-style 'DEFAULT
        ess-eval-visibly t))

(provide 'init-r)
