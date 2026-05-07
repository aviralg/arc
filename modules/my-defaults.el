;;; modules/my-defaults.el --- Core Emacs behavior -*- lexical-binding: t; -*-

;; Naming convention used across all modules:
;;   my/name   — public interactive commands (M-x visible)
;;   my--name  — private helper functions and hooks

;;; --- Startup & Behavior ---
;; Suppress startup screen, use y/n instead of yes/no, confirm before
;; quitting, silence the bell, prefer newer source over stale bytecode.
(setq inhibit-startup-screen t
      initial-scratch-message nil
      use-short-answers t
      confirm-kill-emacs #'y-or-n-p
      ring-bell-function #'ignore
      require-final-newline t
      load-prefer-newer t
      sentence-end-double-space nil)

;;; --- File Handling ---
;; Resolve symlinks to avoid duplicate buffers for the same file.
;; Suppress warnings when visiting a file already open under a different name.
(setq find-file-visit-truename t
      find-file-suppress-same-file-warnings t)

;;; --- Scrolling ---
;; Keep 3 lines of context at screen edges. scroll-conservatively 101
;; prevents Emacs from recentering the cursor (scrolls minimally).
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum nil)

;;; --- Performance ---
;; Increase subprocess read buffer (benefits LSP/eglot).
;; Prevent GC-triggered font cache compaction (trades memory for speed).
;; Skip font-lock during active input (major cursor movement speedup).
;; Disable bidirectional text scanning for LTR-only text.
(setq read-process-output-max (* 4 1024 1024)
      inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;;; --- Long Lines ---
;; Detect files with very long lines (minified JS, logs) and disable
;; expensive features that would freeze Emacs.
(global-so-long-mode 1)

;;; --- Encoding ---
;; UTF-8 as the default for all file I/O, subprocess communication,
;; terminal encoding, and line endings (LF, not CRLF).
(prefer-coding-system 'utf-8-unix)

;;; --- Editorconfig ---
;; Respect .editorconfig files for per-project indent style, tab width,
;; line endings, etc. Built-in since Emacs 30.
(use-package editorconfig
  :ensure nil
  :demand t
  :config
  (editorconfig-mode 1))

;;; --- Repeat Mode ---
;; After pressing a repeatable key (e.g., C-x o), continue with just
;; the last key (o, o, o...) without re-pressing the prefix.
(use-package repeat
  :ensure nil
  :demand t
  :config
  (repeat-mode 1))

;;; --- Which Key ---
;; Show available keybindings in a popup after pressing a prefix key.
(use-package which-key
  :ensure nil
  :demand t
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

(provide 'my-defaults)
;;; modules/my-defaults.el ends here
