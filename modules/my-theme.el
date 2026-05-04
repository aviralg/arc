;;; modules/my-theme.el --- Theme and mode line -*- lexical-binding: t; -*-

;; Modus Operandi — a light, accessible theme built into Emacs.
;; Enables italics for comments, bold for keywords, and mixed fonts
;; for org-mode (variable-pitch prose + fixed-pitch code).
(use-package modus-themes
  :ensure nil
  :demand t
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)
  (load-theme 'modus-operandi :no-confirm))

;;; ---- Mode Line ----
;; Compact mode line showing column number, buffer size, and current
;; function name (useful for navigating large files).

(setq mode-line-compact 'long)
(column-number-mode 1)
(size-indication-mode 1)
;; Show current function name in mode line. Restricted to prog-mode
;; because imenu scanning on every cursor move is expensive in large
;; non-code buffers (org, markdown, shell).
(add-hook 'prog-mode-hook #'which-function-mode)
;; Show empty string instead of "???" when outside a function,
;; so the mode line doesn't change width between function/top-level.
(setq which-func-unknown "")

(provide 'my-theme)
;;; modules/my-theme.el ends here
