;;; modules/my-git.el --- Version control -*- lexical-binding: t; -*-

;; Full-featured git interface. Opens in the current window.
(use-package magit
  :ensure t
  :demand t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-status-goto-file-position t))

;; Use the current frame for ediff instead of spawning a new one.
;; Horizontal split shows files side-by-side.
(use-package ediff
  :ensure nil
  :demand t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; Show git change indicators (added/modified/deleted) in the fringe.
;; Flydiff mode updates indicators without saving (on-the-fly).
;; Refreshes after magit operations.
(use-package diff-hl
  :ensure t
  :demand t
  :after magit
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;; Auto-center window when jumping between hunks with diff-hl-next-hunk
  ;; and diff-hl-previous-hunk (C-x v ] and C-x v [ by default).
  (setq diff-hl-next-previous-hunk-auto-recenter t))

(provide 'my-git)
;;; modules/my-git.el ends here
