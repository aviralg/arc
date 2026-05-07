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

;;; --- Minibuffer ---
;; Allow recursive minibuffer sessions (needed for embark inside
;; minibuffer). Show depth indicator when nested. Prevent cursor
;; from entering the read-only prompt text — requires BOTH the
;; cursor-intangible property AND cursor-intangible-mode active.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; --- File Handling ---
;; Resolve symlinks to avoid duplicate buffers for the same file.
;; Suppress warnings when visiting a file already open under a different name.
;; Preserve system clipboard before Emacs kills overwrite it.
(setq find-file-visit-truename t
      find-file-suppress-same-file-warnings t
      save-interprogram-paste-before-kill t)

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

;;; --- Version Control ---
;; Only check Git, skip SVN/Hg/etc. on every file open.
(setq vc-handled-backends '(Git))

;;; --- Long Lines ---
;; Detect files with very long lines (minified JS, logs) and disable
;; expensive features that would freeze Emacs.
(global-so-long-mode 1)

;;; --- Encoding ---
;; UTF-8 as the default for all file I/O, subprocess communication,
;; terminal encoding, and line endings (LF, not CRLF).
(prefer-coding-system 'utf-8-unix)

;;; --- Fonts ---
;; Set default, fixed-pitch, and variable-pitch fonts. Guarded for
;; terminal mode (no fonts) and daemon mode (no frame at init time).
;; In daemon mode, the hook fires for every new frame including
;; terminal frames (emacsclient -nw) — only set fonts on GUI frames.
(defun my--setup-fonts ()
  (when (find-font (font-spec :family "NewComputerModernMono10"))
    (set-face-attribute 'default nil :family "NewComputerModernMono10" :height 180)
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
