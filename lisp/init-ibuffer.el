;; init-ibuffer.el --- Initialize IBuffer -*- lexical-binding: t -*-
;;; Commentary:
;;;

;;; Buffers

(setq uniquify-buffer-name-style 'forward)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;; Code:
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; https://github.com/muffinmad/emacs-ibuffer-project
(use-package ibuffer-project
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              "Group ibuffer's list by project."
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
  :init
  (setq ibuffer-project-use-cache t))

;; https://github.com/seagle0128/nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :after (nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
