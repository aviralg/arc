;;; modules/my-theme.el --- Theme, fonts, and mode line -*- lexical-binding: t; -*-

;;; --- Theme ---

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

;;; --- Fonts ---
;; Set default, fixed-pitch, and variable-pitch fonts AFTER the theme
;; loads — load-theme resets face attributes, so fonts set before it
;; would be overridden.
;; Guarded for terminal mode (no fonts) and daemon mode (no frame at
;; init time). In daemon mode, the hook fires for every new frame
;; including terminal frames (emacsclient -nw) — only set fonts on
;; GUI frames.
(defun my--setup-fonts ()
  (when (find-font (font-spec :family "NewComputerModernMono10"))
    (set-face-attribute 'default nil :family "NewComputerModernMono10" :height 220)
    (set-face-attribute 'fixed-pitch nil :family "NewComputerModernMono10"))
  (when (find-font (font-spec :family "NewComputerModern10"))
    (set-face-attribute 'variable-pitch nil :family "NewComputerModern10")))
(if (daemonp)
    (progn
      (defun my--setup-fonts-once (frame)
        (when (display-graphic-p frame)
          (with-selected-frame frame (my--setup-fonts))
          (remove-hook 'after-make-frame-functions #'my--setup-fonts-once)))
      (add-hook 'after-make-frame-functions #'my--setup-fonts-once))
  (when (display-graphic-p)
    (my--setup-fonts)))

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
