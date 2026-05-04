;;; init.el --- Minimal Emacs configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; A fast, minimal Emacs config that maximizes built-in features and adds
;; a small number of carefully chosen packages for modern completion,
;; navigation, and development workflows.
;;
;; Requires Emacs 30+ (uses editorconfig, keymap-set, treesit, use-short-answers).
;;
;; External tool dependencies:
;;   Required: rg (ripgrep) — used by xref, grep, consult-ripgrep
;;   Required: fd — used by consult-fd, project-find-file
;;   Optional: gls (GNU coreutils) — used by dired for --group-directories-first
;;   Optional: hunspell — used by ispell for spell checking
;;   Optional: clangd (LLVM) — LSP server for C/C++
;;   Optional: pyright — LSP server for Python (pip install pyright)
;;
;; Install on macOS: brew install ripgrep fd coreutils hunspell llvm

;;; Code:

;;; =========================================================================
;;;; ---- Package Management ----
;;; =========================================================================
;; Configure ELPA/MELPA archives and install any missing packages on first
;; launch. The explicit package list is the single source of truth for
;; external dependencies — use-package handles configuration, not installation.

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

(defvar my--required-packages
  '(no-littering vertico orderless marginalia consult embark embark-consult
    corfu cape magit avy ace-window popper casual-suite markdown-mode
    swift-mode wgrep diff-hl diredfl crux consult-eglot exec-path-from-shell
    pet symbol-overlay))

;; Refresh archives and install anything missing (typically first launch only)
(let ((missing (seq-remove #'package-installed-p my--required-packages)))
  (when missing
    (package-refresh-contents)
    (dolist (pkg missing)
      (package-install pkg))))

;;; =========================================================================
;;;; ---- Dependency Check ----
;;; =========================================================================
;; Verify that external tools and tree-sitter grammars are installed.
;; Runs once at startup and displays a summary buffer listing anything
;; missing. This centralizes all checks in one place instead of scattering
;; guards throughout the config.

(defvar my--required-tools
  '(("rg"       . "Required: used by xref, grep, consult-ripgrep (brew install ripgrep)")
    ("fd"       . "Required: used by consult-fd, project-find-file (brew install fd)")
    ("gls"      . "Optional: used by dired for --group-directories-first (brew install coreutils)")
    ("hunspell" . "Optional: used by ispell/flyspell for spell checking (brew install hunspell)")
    ("clangd"   . "Optional: LSP server for C/C++ (brew install llvm)")
    ("pyright"  . "Optional: LSP server for Python (pip install pyright)")
    ("python3"  . "Optional: used by python-shell for REPL (usually pre-installed)")))

(defvar my--required-grammars
  '(c cpp python json toml yaml cmake bash))

(defun my--check-dependencies ()
  "Check for missing external tools and tree-sitter grammars.
Display a summary buffer if anything is missing."
  (let ((missing-tools
         (seq-filter (lambda (tool) (not (executable-find (car tool))))
                     my--required-tools))
        (missing-grammars
         (when (featurep 'treesit)
           (seq-filter (lambda (lang) (not (treesit-language-available-p lang)))
                       my--required-grammars))))
    (when (or missing-tools missing-grammars)
      (with-current-buffer (get-buffer-create "*Setup Checklist*")
        (erase-buffer)
        (insert "Minimal Emacs Config — Missing Dependencies\n")
        (insert (make-string 45 ?=) "\n\n")
        (when missing-tools
          (insert "MISSING TOOLS:\n\n")
          (dolist (tool missing-tools)
            (insert (format "  ✗ %-10s — %s\n" (car tool) (cdr tool))))
          (insert "\n"))
        (when missing-grammars
          (insert "MISSING TREE-SITTER GRAMMARS:\n\n")
          (dolist (lang missing-grammars)
            (insert (format "  ✗ %s\n" lang)))
          (insert "\n  Install all with:\n")
          (insert "    M-x treesit-install-language-grammar RET <name> RET\n")
          (insert "\n  Or install all at once by evaluating:\n")
          (insert "    (mapc #'treesit-install-language-grammar\n")
          (insert (format "           '%s)\n" missing-grammars)))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer))))))

(add-hook 'emacs-startup-hook #'my--check-dependencies)

;;; =========================================================================
;;;; ---- File Hygiene (No Littering) ----
;;; =========================================================================
;; Redirect all package-generated files into two subdirectories:
;;   state/  — mutable runtime data (history, caches, auto-saves, backups)
;;   config/ — rarely-changing settings (custom.el, templates)
;; Loaded early so subsequent packages pick up the redirected paths.

(use-package no-littering
  :demand t
  :init
  (setq no-littering-var-directory
        (expand-file-name "state/" user-emacs-directory))
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  :config
  ;; Redirect auto-saves and backups into state/. Also handles TRAMP
  ;; and /tmp/ files correctly (keeps them local to avoid leaking data).
  (no-littering-theme-backups)
  ;; Numbered backups keep the last 5 versions as a safety net.
  (setq version-control t
        kept-new-versions 5
        kept-old-versions 2
        delete-old-versions t
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; Load custom.el if it exists (customize writes here, not init.el)
  (when (file-exists-p custom-file)
    (load custom-file)))

;;; =========================================================================
;;;; ---- macOS PATH Inheritance ----
;;; =========================================================================
;; GUI Emacs on macOS doesn't inherit the shell's PATH, so tools like rg,
;; fd, node, python may not be found. This copies PATH, MANPATH, etc.
;; from the user's default shell.

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

;;; =========================================================================
;;;; ---- General Defaults ----
;;; =========================================================================
;; Core Emacs behavior: startup, minibuffer, file handling, encoding,
;; scrolling, performance, editing, window layout, and font configuration.

(use-package emacs
  :ensure nil
  :demand t
  :config

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
  ;; Suppress warnings when symlinks point to the same target.
  ;; Preserve system clipboard before Emacs kills overwrite it.
  (setq find-file-visit-truename t
        find-file-suppress-same-file-warnings t
        save-interprogram-paste-before-kill t)

  ;;; --- Compilation ---
  ;; Auto-scroll compilation output and stop at the first error.
  ;; Process ANSI color codes so build tool output renders correctly.
  ;; ansi-color-compilation-filter is autoloaded — no require needed.
  (setq compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;;; --- Scrolling ---
  ;; Keep 3 lines of context at screen edges. scroll-conservatively 101
  ;; prevents Emacs from recentering the cursor (scrolls minimally).
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        fast-but-imprecise-scrolling t)

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

  ;;; --- Editing ---
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

  ;;; --- Undo ---
  ;; Increase undo limits beyond defaults (160KB/240KB/24MB) so large
  ;; operations (wgrep, refactors) don't lose undo history.
  ;; Uses absolute values so re-evaluating init.el is safe.
  (setq undo-limit (* 640 1024)
        undo-strong-limit (* 960 1024)
        undo-outer-limit (* 96 1024 1024))

  ;;; --- Whitespace ---
  ;; Show trailing whitespace in code and text buffers so it's visible
  ;; before save-time cleanup. Disabled in special buffers (eshell, term)
  ;; where trailing whitespace is normal.
  (defun my--show-trailing-whitespace ()
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook #'my--show-trailing-whitespace)
  (add-hook 'text-mode-hook #'my--show-trailing-whitespace)

  ;;; --- Whitespace Cleanup ---
  ;; Delete trailing whitespace on save, but only in code and text buffers
  ;; (avoids mangling binary files or vendored code).
  (defun my--enable-trailing-whitespace-cleanup ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  (add-hook 'prog-mode-hook #'my--enable-trailing-whitespace-cleanup)
  (add-hook 'text-mode-hook #'my--enable-trailing-whitespace-cleanup)

  ;;; --- Encoding ---
  ;; UTF-8 as the default for all file I/O, subprocess communication,
  ;; and terminal encoding.
  (set-default-coding-systems 'utf-8)

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
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                    (with-selected-frame frame (my--setup-fonts)))))
    (when (display-graphic-p)
      (my--setup-fonts))))

;;; =========================================================================
;;;; ---- Built-in Modes ----
;;; =========================================================================
;; Enable and configure built-in Emacs modes that improve the editing
;; experience without requiring external packages.

;; Respect .editorconfig files for per-project indent style, tab width,
;; line endings, etc. Built-in since Emacs 30.
(use-package editorconfig
  :ensure nil
  :demand t
  :config
  (editorconfig-mode 1))

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

;; Auto-close brackets, quotes, and parens in code buffers.
(use-package electric
  :ensure nil
  :demand t
  :hook (prog-mode . electric-pair-mode))

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

;; After pressing a repeatable key (e.g., C-x o), continue with just
;; the last key (o, o, o...) without re-pressing the prefix.
(use-package repeat
  :ensure nil
  :demand t
  :config
  (repeat-mode 1))

;; Treat CamelCase words as separate words for navigation and editing.
(use-package subword
  :ensure nil
  :demand t
  :hook (prog-mode . subword-mode))

;;; =========================================================================
;;;; ---- Buffer & Keybinding Helpers ----
;;; =========================================================================

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

;; Show available keybindings in a popup after pressing a prefix key.
(use-package which-key
  :ensure nil
  :demand t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))


;;; =========================================================================
;;;; ---- Overlay Highlighting ----
;;; =========================================================================
;;; Paint arbitrary regions with a chosen face -------
;; hi-lock highlights every occurrence of a pattern.  These overlay functions
;; let you color exactly the current line or the active region, one spot only.
;; Highlights are session-only (not saved to disk).
;;
;; my/highlight-line   → color current line (prompts for face)
;; my/highlight-region → color active region (prompts for face)
;; my/clear-highlights → remove all overlay highlights in buffer

(defvar my/highlight-faces
  '(hi-yellow hi-pink hi-green hi-blue hi-salmon
    hi-aquamarine hi-black-b hi-blue-b hi-red-b hi-green-b hi-black-hb)
  "Hi-lock faces available for overlay highlighting.")

(defun my/highlight-line (face)
  "Highlight the current line with FACE (session-only overlay)."
  (interactive (list (intern (completing-read "Face: " my/highlight-faces nil t))))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face     face)
    (overlay-put ov 'category 'my-highlight)))

(defun my/highlight-region (face)
  "Highlight the active region with FACE (session-only overlay)."
  (interactive (list (intern (completing-read "Face: " my/highlight-faces nil t))))
  (when (region-active-p)
    (let ((ov (make-overlay (region-beginning) (region-end))))
      (overlay-put ov 'face     face)
      (overlay-put ov 'category 'my-highlight))
    (deactivate-mark)))

(defun my/clear-highlights ()
  "Remove all overlay highlights in the current buffer."
  (interactive)
  (remove-overlays nil nil 'category 'my-highlight))

;;; =========================================================================
;;;; ---- Server ----
;;; =========================================================================
;; Start the Emacs server so emacsclient can open files instantly
;; from the terminal without launching a new Emacs process.
;; Usage: emacsclient -n file.txt

(use-package server
  :ensure nil
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

;;; =========================================================================
;;;; ---- Utility Commands (Crux) ----
;;; =========================================================================
;; Smart editing commands that replace default Emacs bindings:
;; C-a → move to indentation first, then to column 0
;; C-k → kill to end of line, or kill empty line entirely
;; C-o → open line below with correct indentation
;; C-S-o → open line above
;; C-c d → duplicate line or region
;; C-c D → delete file and its buffer
;; C-c r → rename file and its buffer

(use-package crux
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

;;; =========================================================================
;;;; ---- Modules ----
;;; =========================================================================

(load (expand-file-name "modules/completion" user-emacs-directory))
(load (expand-file-name "modules/theme" user-emacs-directory))
(load (expand-file-name "modules/windows" user-emacs-directory))
(load (expand-file-name "modules/search" user-emacs-directory))
(load (expand-file-name "modules/code" user-emacs-directory))
(load (expand-file-name "modules/git"    user-emacs-directory))
(load (expand-file-name "modules/dired"  user-emacs-directory))
(load (expand-file-name "modules/org"    user-emacs-directory))
(load (expand-file-name "modules/shell"    user-emacs-directory))
(load (expand-file-name "modules/spelling"   user-emacs-directory))
(load (expand-file-name "modules/navigation" user-emacs-directory))
(load (expand-file-name "modules/casual"     user-emacs-directory))
(load (expand-file-name "modules/languages"  user-emacs-directory))

;;; =========================================================================
;;;; ---- Cheatsheet ----
;;; =========================================================================
;; Interactive keybinding browser. C-c ? opens the cheatsheet buffer.
;; cheatsheet.el defines the UI; cheatsheet-entries.el registers all entries.

(load (expand-file-name "cheatsheet" user-emacs-directory))
(load (expand-file-name "cheatsheet-entries" user-emacs-directory))

;;; =========================================================================
;;;; ---- Startup Time ----
;;; =========================================================================
;; Report how long Emacs took to start and how many GC collections
;; occurred. Displayed in the echo area after init completes.

(defun my--display-startup-time ()
  (message "Emacs loaded in %.2fs with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'my--display-startup-time)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
