;;; modules/buffers.el --- Buffer persistence and management -*- lexical-binding: t; -*-

;; Persist minibuffer history (M-x, search, etc.) across sessions.
;; Also saves consult history for smarter completion ranking.
;; Note: corfu-history is added separately in corfu's :config block
;; to ensure the variable is defined before savehist restores it.
(use-package savehist
  :ensure nil
  :demand t
  :config
  (savehist-mode 1)
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring
          consult--buffer-history consult--grep-history consult--find-history)))

;; Track recently opened files for quick access via consult-buffer.
;; Excludes no-littering directories and elpa/ to keep the list clean.
(use-package recentf
  :ensure nil
  :demand t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude
        `(,(regexp-quote (no-littering-expand-var-file-name ""))
          ,(regexp-quote (no-littering-expand-etc-file-name ""))
          ,(regexp-quote (expand-file-name "elpa/" user-emacs-directory)))))

;; Remember cursor position in each file across sessions.
(use-package saveplace
  :ensure nil
  :demand t
  :config
  (save-place-mode 1))

;; Auto-save bookmarks after every change so they survive crashes.
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1))

;; Automatically reload files when changed on disk (e.g., by git).
;; Uses file-system notifications instead of polling for efficiency.
(use-package autorevert
  :ensure nil
  :demand t
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 5
        auto-revert-avoid-polling t
        auto-revert-check-vc-info nil
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Disambiguate buffer names by appending parent directory path
;; (e.g., "init.el|config/" instead of "init.el<2>").
(use-package uniquify
  :ensure nil
  :demand t
  :config
  (setq uniquify-buffer-name-style 'post-forward))

;; Replace list-buffers with ibuffer — grouped, filterable, and
;; much more capable. consult-buffer (C-x b) handles quick switching;
;; ibuffer (C-x C-b) is for bulk operations (kill, save, mark).
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;;; modules/buffers.el ends here
