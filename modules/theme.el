;;; modules/theme.el --- Theme and mode line -*- lexical-binding: t; -*-

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
(which-function-mode 1)
;; Show empty string instead of "???" when outside a function,
;; so the mode line doesn't change width between function/top-level.
(setq which-func-unknown "")

;;; modules/theme.el ends here
