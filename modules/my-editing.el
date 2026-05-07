;;; modules/my-editing.el --- Text editing -*- lexical-binding: t; -*-

;;; --- Cursor & Indentation ---
;; Thin bar cursor. Spaces instead of tabs. 4-space indent. 80-col fill.
;; Keep underlines at the font's descent line to prevent line height shifts
;; when the cursor moves over underlined text (e.g., matching parens, links).
(setq x-underline-at-descent-line t
      ns-use-underline-at-descent-line t)
;; Small line-spacing prevents text baseline shifts on macOS when
;; face backgrounds are applied (e.g., vertico highlight, show-paren).
;; The NS text renderer recalculates line metrics on face changes;
;; pre-allocating padding avoids the rounding that causes the shift.
(blink-cursor-mode -1)
(setq-default cursor-type 'bar
              ;; line-spacing 0.15
              )
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;;; --- Undo & Clipboard ---
;; Increase undo limits beyond defaults (160KB/240KB/24MB) so large
;; operations (wgrep, refactors) don't lose undo history.
;; Uses absolute values so re-evaluating init.el is safe.
;; Preserve system clipboard before Emacs kills overwrite it.
(setq undo-limit (* 640 1024)
      undo-strong-limit (* 960 1024)
      undo-outer-limit (* 96 1024 1024)
      save-interprogram-paste-before-kill t)

;;; --- Whitespace ---
;; Show trailing whitespace in code and text buffers so it's visible
;; before save-time cleanup. Disabled in special buffers (eshell, term)
;; where trailing whitespace is normal.
(defun my--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'my--show-trailing-whitespace)
(add-hook 'text-mode-hook #'my--show-trailing-whitespace)

;;; --- Whitespace Cleanup ---
;; Delete trailing whitespace on manual save, but not during
;; auto-save-visited-mode's periodic saves (which would yank
;; whitespace out from under you mid-edit).
;; The guard checks this-command: auto-save's timer doesn't set it
;; to save-buffer, so the cleanup is skipped during auto-saves.
(defun my--delete-trailing-whitespace-manually ()
  (when (memq this-command '(save-buffer save-some-buffers))
    (delete-trailing-whitespace)))

(defun my--enable-trailing-whitespace-cleanup ()
  (add-hook 'before-save-hook #'my--delete-trailing-whitespace-manually nil t))
(add-hook 'prog-mode-hook #'my--enable-trailing-whitespace-cleanup)
(add-hook 'text-mode-hook #'my--enable-trailing-whitespace-cleanup)

;; Auto-close brackets, quotes, and parens in code buffers.
(use-package elec-pair
  :ensure nil
  :demand t
  :hook (prog-mode . electric-pair-local-mode))

;; Highlight matching parenthesis with minimal delay (50ms).
(use-package paren
  :ensure nil
  :demand t
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.05
        show-paren-when-point-inside-paren t))

;; Typing while text is selected replaces the selection (standard
;; behavior in every modern editor).
(use-package delsel
  :ensure nil
  :demand t
  :config
  (delete-selection-mode 1))

;; Treat CamelCase words as separate words for navigation and editing.
(use-package subword
  :ensure nil
  :demand t
  :hook (prog-mode . subword-mode))

;;; ---- Overlay Highlighting ----
;; Paint arbitrary regions with a chosen face.
;; hi-lock highlights every occurrence of a pattern.  These overlay functions
;; let you color exactly the current line or the active region, one spot only.
;; Highlights are session-only (not saved to disk).
;;
;; my/highlight-line   → color current line (prompts for face)
;; my/highlight-region → color active region (prompts for face)
;; my/clear-highlights → remove all overlay highlights in buffer

(require 'hi-lock)

(defun my/highlight-faces ()
  "Return all hi-lock highlight faces defined in the current theme."
  (seq-filter (lambda (f) (string-prefix-p "hi-" (symbol-name f)))
              (face-list)))

(defun my/highlight-line (face)
  "Highlight the current line with FACE (session-only overlay)."
  (interactive (list (intern (completing-read "Face: " (my/highlight-faces) nil t))))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face     face)
    (overlay-put ov 'category 'my-highlight)))

(defun my/highlight-region (face)
  "Highlight the active region with FACE (session-only overlay)."
  (interactive (list (intern (completing-read "Face: " (my/highlight-faces) nil t))))
  (when (region-active-p)
    (let ((ov (make-overlay (region-beginning) (region-end))))
      (overlay-put ov 'face     face)
      (overlay-put ov 'category 'my-highlight))
    (deactivate-mark)))

(defun my/clear-highlights ()
  "Remove all overlay highlights in the current buffer."
  (interactive)
  (remove-overlays nil nil 'category 'my-highlight))

;;; ---- Utility Commands (Crux) ----
;; Smart editing commands that replace default Emacs bindings:
;; C-a → move to indentation first, then to column 0
;; C-k → kill to end of line, or kill empty line entirely
;; C-o → open line below with correct indentation
;;       (in dired/ibuffer/etc., C-o is overridden by casual-suite
;;       to open a transient menu; dired-display-file is available
;;       inside that menu)
;; C-S-o → open line above
;; C-c d → duplicate line or region
;; C-c D → delete file and its buffer
;; C-c r → rename file and its buffer
(use-package crux
  :ensure t
  :demand t
  :bind (("C-a"     . crux-move-beginning-of-line)
         ("C-k"     . crux-smart-kill-line)
         ("C-o"     . crux-smart-open-line)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c D"   . crux-delete-file-and-buffer)
         ("C-c r"   . crux-rename-file-and-buffer))
  :config
  ;; Auto-reopen files as root when hitting permission errors
  (crux-reopen-as-root-mode 1))

;;; ---- Zen Mode ----
;; Distraction-free writing: centered text, hidden mode line.
;; Toggle with C-c z.
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 120))

(defvar my--zen-mode-hooks '(text-mode-hook org-mode-hook prog-mode-hook)
  "Hooks where zen mode enables olivetti and hides the mode line.")

(defun my--zen-mode-activate ()
  "Enable olivetti and hide mode line in the current buffer."
  (olivetti-mode 1)
  (setq-local mode-line-format nil)
  (force-mode-line-update))

(defun my--zen-mode-deactivate ()
  "Disable olivetti and restore mode line in the current buffer."
  (olivetti-mode -1)
  (setq-local mode-line-format (default-value 'mode-line-format))
  (force-mode-line-update))

(defvar my--zen-mode-active nil
  "Non-nil when zen mode is globally active.")

(defun my/zen-mode ()
  "Toggle zen mode across all text, org, and prog buffers."
  (interactive)
  (if my--zen-mode-active
      (progn
        (dolist (hook my--zen-mode-hooks)
          (remove-hook hook #'my--zen-mode-activate))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when olivetti-mode
              (my--zen-mode-deactivate))))
        (setq my--zen-mode-active nil)
        (message "Zen mode disabled."))
    (dolist (hook my--zen-mode-hooks)
      (add-hook hook #'my--zen-mode-activate))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'text-mode 'org-mode 'prog-mode)
          (my--zen-mode-activate))))
    (setq my--zen-mode-active t)
    (message "Zen mode enabled.")))

(keymap-global-set "C-c z" #'my/zen-mode)

(provide 'my-editing)
;;; modules/my-editing.el ends here
