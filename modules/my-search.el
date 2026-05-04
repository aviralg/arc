;;; modules/my-search.el --- Search and grep -*- lexical-binding: t; -*-

;; Show match count (e.g., "3/17") during incremental search.
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "))

;; Replace built-in grep with ripgrep for M-x grep and M-x grep-find.
;; Both commands use the same rg invocation since rg recurses by default.
;; Cursor position 27 places point between the single quotes in -e ''.
;; Update this number if you change the flags.
(use-package grep
  :ensure nil
  :config
  (setq grep-program "rg"
        grep-use-null-device nil)
  (grep-apply-setting 'grep-command '("rg -n -H --no-heading -e '' ." . 27))
  (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e '' ." . 27)))

;; Edit grep/ripgrep results in-place. Changes are applied to the
;; original files when you finish (C-c C-e). Modified buffers are left
;; unsaved for manual review — save individually with C-x C-s or all
;; with C-x s. Versioned backups in state/backup/ provide a safety net.
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer nil))

(provide 'my-search)
;;; modules/my-search.el ends here
