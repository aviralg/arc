;;; modules/my-windows.el --- Window management -*- lexical-binding: t; -*-

;;; --- Window Display Rules ---
;; Control where non-popup buffers appear. Popup buffers (help, grep,
;; compilation, etc.) are managed by popper instead.
(setq display-buffer-alist
      `(;; Shells appear at the bottom (matches *eshell*<2>, *eshell: proj*, etc.)
        (,(rx bos (or "*eshell" "*shell" "*term"))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom) (slot . 0) (window-height . 0.3))
        ;; Info documentation opens on the right
        ("\\*info\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right) (slot . 0) (window-width . 0.4))
        ;; Magit reuses the current window
        ("magit:"
         (display-buffer-reuse-window display-buffer-same-window))))

;;; --- Window Splitting ---
;; Prefer vertical splits (side-by-side) on wide monitors.
;; Pixel-wise window resizing for precise sizing.
(setq split-width-threshold 170
      window-resize-pixelwise t)

;; Undo/redo window configurations with C-c left / C-c right.
(use-package winner
  :ensure nil
  :demand t
  :config
  (winner-mode 1))

;; Switch to any visible window by number. M-1 through M-9 jump
;; directly; the window number is shown in each window's mode line.
;; Overrides digit-argument on M-1..M-9. Numeric prefix args are
;; still available via C-u N (e.g., C-u 3 C-k) or C-0..C-9.
(use-package ace-window
  :ensure t
  :demand t
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
        aw-scope 'frame)
  (ace-window-display-mode 1)
  (defun my--select-window-by-number (n)
    "Select the Nth window in `aw-window-list' order."
    (let ((win (nth (1- n) (aw-window-list))))
      (when win (aw-switch-to-window win))))
  (dotimes (i 9)
    (let ((n (1+ i)))
      (defalias (intern (format "my/select-window-%d" n))
        (lambda () (interactive) (my--select-window-by-number n))
        (format "Select window %d." n))
      (keymap-global-set
       (format "M-%d" n)
       (intern (format "my/select-window-%d" n))))))

;;; ---- Popup Management (Popper) ----
;; Classify certain buffers as popups that can be toggled, cycled, and
;; dismissed with consistent keybindings. Groups popups by project root.
(use-package popper
  :ensure t
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (popper-mode 1)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          "\\*xref\\*"
          "\\*eldoc\\*"
          "\\*grep\\*"
          "\\*ripgrep\\*"
          "\\*Occur\\*"
          "\\*Embark Collect\\*"
          "\\*Embark Export\\*"
          help-mode
          compilation-mode
          occur-mode
          flymake-diagnostics-buffer-mode))
  ;; Show available popup actions in echo area
  (setq popper-echo-dispatch-actions t)
  ;; Group popups by project root
  (setq popper-group-function #'popper-group-by-project))

(provide 'my-windows)
;;; modules/my-windows.el ends here
