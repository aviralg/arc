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

  ;;; --- Window Display Rules ---
  ;; Control where non-popup buffers appear. Popup buffers (help, grep,
  ;; compilation, etc.) are managed by popper instead.
  (setq display-buffer-alist
        `(;; Shells appear at the bottom
          (,(rx (or "*eshell*" "*shell*" "*term*"))
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

;; Undo/redo window configurations with C-c left / C-c right.
(use-package winner
  :ensure nil
  :demand t
  :config
  (winner-mode 1))

;; Switch to any visible window by number. M-1 through M-9 jump
;; directly; the window number is shown in each window's mode line.
;; Overrides digit-argument on M-1..M-9 — use C-u for prefix args.
(use-package ace-window
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
      (keymap-global-set
       (format "M-%d" n)
       (lambda () (interactive) (my--select-window-by-number n))))))

;;; =========================================================================
;;;; ---- Popup Management (Popper) ----
;;; =========================================================================
;; Classify certain buffers as popups that can be toggled, cycled, and
;; dismissed with consistent keybindings. Groups popups by project root.

(use-package popper
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
;;;; ---- Theme ----
;;; =========================================================================
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

;;; =========================================================================
;;;; ---- Minibuffer Completion ----
;;; =========================================================================
;; Modern completion stack: vertico (vertical UI), orderless (fuzzy
;; matching), marginalia (annotations), embark (contextual actions),
;; and consult (enhanced commands with live preview).

;; Vertical minibuffer completion UI with cycling and directory
;; navigation (RET enters dirs, DEL goes up).
(use-package vertico
  :demand t
  :config
  (vertico-mode 1)
  (setq vertico-cycle t
        vertico-count 12
        ;; Prioritize history matches, then shorter candidates, then alpha
        vertico-sort-function #'vertico-sort-history-length-alpha)
  (require 'vertico-directory)
  (keymap-set vertico-map "RET"   #'vertico-directory-enter)
  (keymap-set vertico-map "DEL"   #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  ;; Clean up file path when shadowed by a new input (e.g., ~/ after /tmp/)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; Recall previous completion sessions with M-R
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (keymap-global-set "M-R" #'vertico-repeat)
  ;; Per-command display modes (toggle with M-B, M-F, M-G in minibuffer)
  (require 'vertico-multiform)
  (vertico-multiform-mode 1)
  ;; Quick key selection in minibuffer (like corfu-quick for completions)
  (require 'vertico-quick)
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))

;; Space-separated completion matching — "buf init" matches "consult-buffer"
;; and "init.el". Falls back to basic for non-orderless-aware commands.
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;; Rich annotations in the minibuffer (file sizes, docstrings, etc.).
(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

;; Contextual actions on any minibuffer candidate or buffer target.
;; C-. to act, C-; for default action, C-h B to explore all bindings.
;; Also replaces the default prefix-help with embark's richer version.
(use-package embark
  :demand t
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command
        embark-cycle-key "C-,"))

;; Enhanced commands that replace built-in equivalents with versions
;; that support live preview, narrowing, and grouping.
;; C-o in isearch jumps to consult-line with the current search term.
;; Preview is manual (M-.) for heavy commands like ripgrep.
(use-package consult
  :demand t
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-fd)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ("M-y"     . consult-yank-pop)
         ("C-x r b" . consult-bookmark)
         :map isearch-mode-map
         ("C-o"     . consult-line))
  :config
  (setq consult-narrow-key "<"
        consult-widen-key ">"
        consult-after-jump-hook '(recenter pulse-momentary-highlight-one-line)
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        ;; Skip previewing TRAMP remote files, encrypted, binary, and compressed files
        consult-preview-excluded-files
        '("\\`/[^/|:]+:" "\\.gpg\\'" "\\.\\(zip\\|tar\\|gz\\|sqlite\\)\\'"
          "\\.\\(pdf\\|exe\\|so\\|dylib\\)\\'"))
  ;; Disable automatic preview for grep commands (use M-. to preview)
  (consult-customize
   consult-ripgrep consult-grep consult-git-grep
   :preview-key "M-."))

;; Integration between embark and consult — provides embark actions
;; for consult commands (e.g., export grep results to a buffer).
(use-package embark-consult
  :demand t
  :after (embark consult))

;;; =========================================================================
;;;; ---- In-Buffer Completion ----
;;; =========================================================================
;; Corfu provides the popup, cape provides additional completion sources,
;; and hippie-expand offers fallback expansion via M-/.

;; Popup completion at point. Auto-triggers after 2 characters with
;; a short delay. History mode ranks frequently used completions higher.
;; Popupinfo shows documentation for the selected candidate.
(use-package corfu
  :demand t
  :config
  (global-corfu-mode 1)
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt)
  (corfu-history-mode 1)
  ;; Add corfu-history to savehist here (not in the savehist block)
  ;; so the variable is defined before savehist tries to restore it.
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.25 . 0.1))
  (require 'corfu-quick)
  (keymap-set corfu-map "M-q" #'corfu-quick-complete)
  (keymap-set corfu-map "C-q" #'corfu-quick-insert))

;; Additional completion-at-point backends: dabbrev (words from open
;; buffers), file paths, and language keywords. Scoped to prog-mode and
;; text-mode to avoid nonsensical completions in special buffers.
;; The cape-wrap advice on eglot fixes issues where eglot's completion
;; would block other backends or return stale results.
(use-package cape
  :demand t
  :config
  (defun my--setup-cape-backends ()
    (add-hook 'completion-at-point-functions #'cape-keyword nil t)
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t))
  (add-hook 'prog-mode-hook #'my--setup-cape-backends)
  (add-hook 'text-mode-hook #'my--setup-cape-backends)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-history nil t)))
  ;; Bust stale eglot completion cache
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Allow cape backends to supplement eglot results
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

;; Skip binary buffers when searching for dabbrev completions.
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Smart expansion via M-/ — tries dabbrev first, then filenames,
;; abbreviations, and lisp symbols in order.
(use-package hippie-exp
  :ensure nil
  :demand t
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;;; =========================================================================
;;;; ---- Code Intelligence ----
;;; =========================================================================
;; Eglot (built-in LSP client), tree-sitter for syntax highlighting,
;; flymake for diagnostics, eldoc for documentation, and xref/grep
;; configured to use ripgrep.

;; LSP client — auto-starts for configured language modes.
;; Shuts down when the last buffer for a project closes.
;; Note: python-ts-mode is not listed here — pet handles eglot startup
;; for Python after configuring the virtual environment (see Python section).
(use-package eglot
  :ensure nil
  :demand t
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (swift-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 20000)
        eglot-sync-connect 3))

;; Search LSP workspace symbols through consult's interface (M-g s).
(use-package consult-eglot
  :demand t
  :after eglot
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)))

;; Tree-sitter grammar sources for configured languages.
;; Install grammars with M-x treesit-install-language-grammar.
;; Level 3 fontification balances detail vs. performance.
(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 3
        treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (bash "https://github.com/tree-sitter/tree-sitter-bash"))))

;; !! DO NOT USE treesit-auto / global-treesit-auto-mode HERE !!
;; treesit-auto calls treesit-language-available-p on EVERY file open
;; to check grammar availability. This adds ~70% CPU overhead per file
;; open (confirmed via profiler). Instead, we build the remap alist
;; ONCE at init time using seq-filter + treesit-language-available-p,
;; which runs the check once per grammar at startup and never again.
;;
;; To add a new language: add it to treesit-language-source-alist above,
;; add a remap entry below, install the grammar with
;; M-x treesit-install-language-grammar, and restart Emacs.
(when (featurep 'treesit)
  (setq major-mode-remap-alist
        (seq-filter
         (lambda (entry)
           (treesit-language-available-p
          (pcase (cdr entry)
            ('python-ts-mode     'python)
            ('c-ts-mode          'c)
            ('c++-ts-mode        'cpp)
            ;; c-or-c++-ts-mode needs both c and cpp grammars
            ('c-or-c++-ts-mode   (and (treesit-language-available-p 'c) 'cpp))
            ('js-json-ts-mode    'json)
            ('json-ts-mode       'json)
            ('yaml-ts-mode       'yaml)
            ('bash-ts-mode       'bash)
            ('cmake-ts-mode      'cmake)
            ('toml-ts-mode       'toml))))
       '((python-mode    . python-ts-mode)
         (c-mode         . c-ts-mode)
         (c++-mode       . c++-ts-mode)
         (c-or-c++-mode  . c-or-c++-ts-mode)
         ;; Emacs 30 built-in modes — these are the actual mode names
         (js-json-mode   . json-ts-mode)
         (conf-toml-mode . toml-ts-mode)
         (sh-mode        . bash-ts-mode))))

  ;; yaml and cmake have no built-in base mode to remap from,
  ;; so associate file extensions directly with the ts-mode.
  (when (treesit-language-available-p 'yaml)
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))))

;; On-the-fly error/warning checking in code buffers.
;; M-n / M-p to jump between errors.
;; M-g f for searchable diagnostics list (only active in flymake-mode buffers).
;; Note: consult-flymake requires consult to be loaded first.
(use-package flymake
  :ensure nil
  :demand t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("M-g f" . consult-flymake)))

;; Show documentation for the symbol at point in the echo area.
;; By default, eldoc picks one documentation source and shows only that.
;; With compose strategy, it combines all sources (eglot type signatures
;; AND flymake diagnostics) so both appear when the cursor is on an error.
;; Allow up to 2 echo area lines so both fit without truncation.
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p 2
        eldoc-idle-delay 0.5
        eldoc-documentation-strategy #'eldoc-documentation-compose))

;; Use ripgrep for xref searches (jump to definition, find references).
(use-package xref
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep))


;;; =========================================================================
;;;; ---- Project Management ----
;;; =========================================================================
;; Built-in project.el for project-aware commands. Uses fd instead of
;; find for file listing when available.

(use-package project
  :ensure nil
  :config
  (setq project-find-functions '(project-try-vc)
        project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-dired "Dired")
          (project-eshell "Eshell")))
  ;; Use fd for faster project file listing (respects .gitignore).
  ;; Only overrides the default when no specific dirs are requested;
  ;; falls back to the default implementation for narrowed scopes.
  ;; Returns absolute paths to satisfy the project-files API contract.
  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head vc)) &optional dirs)
      (let* ((root (project-root project))
             (default-directory root))
        (if (or (null dirs) (equal dirs (list root)))
            (split-string (shell-command-to-string "fd -t f -H -E .git -a") "\n" t)
          (cl-call-next-method))))))

;;; =========================================================================
;;;; ---- File Manager (Dired) ----
;;; =========================================================================
;; Built-in directory editor with colorized output via diredfl.
;; Uses GNU ls (gls) on macOS for --group-directories-first support.

(use-package dired
  :ensure nil
  :config
  ;; Guess target directory from other dired window (for copy/move)
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-kill-when-opening-new-dired-buffer t)
  ;; macOS ships BSD ls which lacks --group-directories-first
  (let ((gls (executable-find "gls")))
    (if gls
        (setq insert-directory-program gls
              dired-listing-switches "-alh --group-directories-first")
      (setq dired-listing-switches "-alh"))))

;; Colorize dired output (file sizes, dates, permissions, etc.).
(use-package diredfl
  :demand t
  :config
  (diredfl-global-mode 1))

;;; =========================================================================
;;;; ---- Git ----
;;; =========================================================================
;; Magit for git operations, ediff for comparing files (in same frame),
;; and diff-hl for fringe indicators showing uncommitted changes.

;; Full-featured git interface. Opens in the current window.
(use-package magit
  :demand t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-status-goto-file-position t))

;; Use the current frame for ediff instead of spawning a new one.
;; Horizontal split shows files side-by-side.
(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; Show git change indicators (added/modified/deleted) in the fringe.
;; Flydiff mode updates indicators without saving (on-the-fly).
;; Refreshes after magit operations.
(use-package diff-hl
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;; Auto-center window when jumping between hunks with diff-hl-next-hunk
  ;; and diff-hl-previous-hunk (C-x v ] and C-x v [ by default).
  (setq diff-hl-next-previous-hunk-auto-recenter t))

;;; =========================================================================
;;;; ---- Navigation ----
;;; =========================================================================
;; Avy for jumping to visible text. Press "." during avy to run
;; embark-act at the target location (avy + embark composition).

(use-package avy
  :demand t
  :bind (("C-'" . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.4
        avy-all-windows t
        avy-background t)
  ;; Register embark as an avy dispatch action
  (defun my--avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'my--avy-action-embark))

;; Highlight all occurrences of the symbol at point. M-i toggles
;; highlighting; then n/p/r in the overlay keymap jump/rename.
(use-package symbol-overlay
  :demand t
  :bind ("M-i" . symbol-overlay-put))

;;; =========================================================================
;;;; ---- Org Mode ----
;;; =========================================================================
;; Personal organization: TODO tracking, note capture, journaling,
;; and agenda views. All org files live under ~/org/.

(use-package org
  :ensure nil
  :demand t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;;; --- Display ---
  ;; Clean visual appearance: indented headings, hidden markup markers,
  ;; syntax-highlighted code blocks, folded to heading level on open.
  (setq org-return-follows-link t
        org-startup-indented t
        org-startup-folded 'content
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-log-done 'time
        org-log-into-drawer t)

  ;;; --- Files ---
  ;; All org files under ~/org/. Agenda scans this directory.
  ;; Create ~/org/ before first use: mkdir -p ~/org
  (setq org-directory "~/org"
        org-agenda-files '("~/org")
        org-default-notes-file "~/org/inbox.org")

  ;;; --- TODO Workflow ---
  ;; TODO → IN-PROGRESS → WAITING → DONE / CANCELLED
  ;; @ suffix prompts for a note (why waiting? why cancelled?)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)")))

  ;;; --- Refile ---
  ;; Move headings to any file in ~/org/, up to 3 levels deep.
  ;; Shows full path in vertico for disambiguation. Auto-saves after.
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (advice-add 'org-refile :after #'org-save-all-org-buffers)

  ;;; --- Capture Templates ---
  ;; C-c c t — new TODO in inbox with timestamp and link back
  ;; C-c c n — quick note
  ;; C-c c j — journal entry filed under today's date
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/inbox.org")
           "* TODO %?\n%U\n%a" :empty-lines 1)
          ("n" "Note" entry (file "~/org/notes.org")
           "* %?\n%U" :empty-lines 1)
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n%U" :empty-lines 1)))

  ;;; --- Agenda ---
  ;; C-c a d — dashboard showing tasks grouped by state
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((todo "IN-PROGRESS" ((org-agenda-overriding-header "In Progress")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO" ((org-agenda-overriding-header "To Do"))))))))

;;; =========================================================================
;;;; ---- Shell ----
;;; =========================================================================
;; Eshell with consult-history integration (C-r for searchable history).

(use-package eshell
  :ensure nil
  :config
  (setq eshell-scroll-to-bottom-on-input 'this
        eshell-destroy-buffer-when-process-dies t)
  ;; Bind C-r to consult-history in eshell for fuzzy history search
  (defun my--eshell-consult-history ()
    (keymap-set eshell-mode-map "C-r" #'consult-history))
  (add-hook 'eshell-mode-hook #'my--eshell-consult-history))

;;; =========================================================================
;;;; ---- Spell Checking ----
;;; =========================================================================
;; Use hunspell as the spell-check backend when available.
;; Auto-enable on-the-fly spell checking in text buffers.

(use-package ispell
  :ensure nil
  :demand t
  :config
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US")))

;; Auto-enable on-the-fly spell checking in text buffers,
;; but only if a spell checker is actually installed.
(use-package flyspell
  :ensure nil
  :demand t
  :config
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)))

;;; =========================================================================
;;;; ---- Language Modes ----
;;; =========================================================================

;;; --- Python ---
;; Uses python-ts-mode (tree-sitter) with eglot for LSP.
;; Eglot auto-detects pyright or pylsp — install one:
;;   pip install pyright   (recommended, faster)
;;   pip install python-lsp-server   (alternative)
(use-package python
  :ensure nil
  :demand t
  :config
  ;; Use python3 for the REPL (C-c C-p) and script execution
  (setq python-shell-interpreter "python3"
        python-indent-offset 4
        python-shell-dedicated 'project))

;; Auto-detect virtual environments from pyproject.toml, .venv,
;; Pipfile, poetry.lock, etc. Sets python-shell-interpreter and
;; configures eglot to use the correct Python for the project.
;; Without this, LSP reports false "module not found" errors.
(use-package pet
  :demand t
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (when-let* ((python (pet-executable-find "python")))
                (setq-local python-shell-interpreter python))
              (when-let* ((root (pet-virtualenv-root)))
                (setq-local python-shell-virtualenv-root root))
              (pet-eglot-setup)
              (eglot-ensure))))

;;; --- C / C++ ---
;; Uses c-ts-mode / c++-ts-mode (tree-sitter) with eglot for LSP.
;; Eglot auto-detects clangd — install via:
;;   brew install llvm   (macOS, includes clangd)
;;   apt install clangd   (Linux)
;; For project-specific settings, place a .clangd file or
;; compile_commands.json in the project root.
(use-package c-ts-mode
  :ensure nil
  :demand t
  :config
  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style 'k&r))

;; Toggle between header and source files with C-c o (e.g., foo.h ↔ foo.cpp).
;; Built-in, works with .h, .hpp, .c, .cpp, .cc, .cxx out of the box.
;; Keybindings deferred via hooks since the keymaps don't exist until
;; a C/C++ file is opened.
(use-package find-file
  :ensure nil
  :demand t
  :config
  (setq ff-always-in-other-window nil
        ff-search-directories '("." "../include" "../src" "../../include" "../../src"))
  (defun my--bind-ff-find-other-file ()
    (keymap-set (current-local-map) "C-c o" #'ff-find-other-file))
  (add-hook 'c-ts-mode-hook #'my--bind-ff-find-other-file)
  (add-hook 'c++-ts-mode-hook #'my--bind-ff-find-other-file))

;; Tree-sitter mode for CMakeLists.txt and .cmake files.
(use-package cmake-ts-mode
  :ensure nil
  :demand t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Markdown editing. Native code-block fontification is disabled because
;; it loads a sub-mode font-lock per fenced block — the dominant cost in
;; profiled redisplay (markdown-match-code + cascading matchers). Remaining
;; font-lock is deferred to idle time so keystroke insertion never blocks.
(use-package markdown-mode
  :demand t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively nil
        markdown-fontify-whole-heading-line t
        markdown-hide-urls t
        markdown-enable-math t)
  (defun my--markdown-defer-fontification ()
    (setq-local jit-lock-defer-time 0))
  (add-hook 'markdown-mode-hook #'my--markdown-defer-fontification))

;; Swift support (non-tree-sitter, since the Swift grammar has build issues).
(use-package swift-mode
  :demand t
  :mode "\\.swift\\'")

;;; Lisp structural editing -----------------------------------------------
;; Four commands built on Emacs sexp primitives — no external packages.
;;
;;   absorb   pull the next sibling sexp inside the current list
;;   expel    push the last sexp out of the current list
;;   unwrap   remove the current list's delimiters, splicing contents
;;   wrap     surround the sexp at point (or region) with parentheses
;;
;; Keybindings (active in all Lisp-family modes via my/lisp-edit-mode):
;;   C-<right>   absorb
;;   C-<left>    expel
;;   M-S         unwrap
;;   M-(         wrap

;; ---- helpers ----

(defun my/lisp--enclosing-open ()
  "Return the position of the opening delimiter of the enclosing list.
Signals `user-error' if point is not inside any list."
  (condition-case nil
      (save-excursion
        (backward-up-list 1 t t)
        (point))
    (scan-error
     (user-error "Not inside any list"))))

(defun my/lisp--enclosing-close ()
  "Return the position of the closing delimiter of the enclosing list.
Signals `user-error' if point is not inside any list."
  (condition-case nil
      (save-excursion
        (up-list 1 t t)
        (1- (point)))
    (scan-error
     (user-error "Not inside any list"))))

;; ---- absorb (slurp forward) ----

(defun my/lisp-absorb ()
  "Pull the next sibling sexp inside the current list.

  (a b c) d  →  (a b c d)"
  (interactive)
  (let* ((close-pos   (my/lisp--enclosing-close))
         (close-char  (char-after close-pos))
         (after-close (1+ close-pos))
         (next-end
          (condition-case nil
              (save-excursion
                (goto-char after-close)
                (skip-chars-forward " \t\n")
                (forward-sexp 1)
                (point))
            (scan-error
             (user-error "absorb: no next sibling sexp")))))
    (let ((gap-and-sexp (delete-and-extract-region after-close next-end)))
      (delete-region close-pos (1+ close-pos))
      (goto-char close-pos)
      (insert gap-and-sexp)
      (insert close-char))))

;; ---- expel (barf forward) ----

(defun my/lisp-expel ()
  "Push the last sexp out of the current list.

  (a b c d)  →  (a b c) d"
  (interactive)
  (let* ((close-pos  (my/lisp--enclosing-close))
         (close-char (char-after close-pos))
         last-sexp-start last-sexp-end)
    (condition-case nil
        (save-excursion
          (goto-char close-pos)     ; point is before ')'
          (backward-sexp 1)         ; scans back across the last element
          (setq last-sexp-start (point))
          (forward-sexp 1)
          (setq last-sexp-end (point)))
      (scan-error
       (user-error "expel: list is empty")))
    (let ((ws-start (save-excursion
                      (goto-char last-sexp-start)
                      (skip-chars-backward " \t\n")
                      (point)))
          (sexp-text (buffer-substring-no-properties
                      last-sexp-start last-sexp-end)))
      (delete-region ws-start (1+ close-pos))
      (goto-char ws-start)
      (insert close-char " " sexp-text))))

;; ---- unwrap (splice) ----

(defun my/lisp-unwrap ()
  "Remove the delimiters of the enclosing list, splicing its contents.

  (a (b c) d) with point in (b c)  →  (a b c d)"
  (interactive)
  (let ((open-pos  (my/lisp--enclosing-open))
        (close-pos (my/lisp--enclosing-close)))
    (delete-region close-pos (1+ close-pos))
    (delete-region open-pos  (1+ open-pos))))

;; ---- wrap ----

(defun my/lisp-wrap ()
  "Surround the sexp at point, or the active region, with parentheses.

  point before b in `a b c'  →  `a (b) c'
  region `b c'               →  `(b c)'

Point is placed just inside the opening paren after the operation."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)   (insert ")")
          (goto-char beg)   (insert "("))
        (deactivate-mark)
        (goto-char (1+ beg)))
    (let* ((sexp-start
            (save-excursion
              (skip-chars-forward " \t\n")
              ;; Normalize to sexp boundary (handles point inside an atom).
              (condition-case nil
                  (progn (forward-sexp 1) (backward-sexp 1))
                (scan-error nil))
              (point)))
           (sexp-end
            (condition-case nil
                (save-excursion
                  (goto-char sexp-start)
                  (forward-sexp 1)
                  (point))
              (scan-error
               (user-error "wrap: no sexp at point")))))
      (save-excursion
        (goto-char sexp-end)   (insert ")")
        (goto-char sexp-start) (insert "("))
      (goto-char (1+ sexp-start)))))

;; ---- transient menu ----

(transient-define-prefix my/lisp-menu ()
  "Sexp navigation and structural editing.
All built-in commands are also available directly via their C-M-* bindings."
  [["Move"
    ("f" "forward sexp"        forward-sexp)
    ("b" "backward sexp"       backward-sexp)
    ("u" "up list"             backward-up-list)
    ("d" "down list"           down-list)
    ("n" "forward list"        forward-list)
    ("p" "backward list"       backward-list)
    ("a" "beginning of defun"  beginning-of-defun)
    ("e" "end of defun"        end-of-defun)]
   ["Select & Edit"
    ("m" "mark sexp"           mark-sexp)
    ("M" "mark defun"          mark-defun)
    ("k" "kill sexp"           kill-sexp)
    ("t" "transpose sexps"     transpose-sexps)
    (";" "comment/uncomment"   comment-dwim)]
   ["Structure"
    (">" "absorb  (a b) c → (a b c)" my/lisp-absorb)
    ("<" "expel   (a b c) → (a b) c" my/lisp-expel)
    ("s" "unwrap  (a (b) c) → (a b c)" my/lisp-unwrap)
    ("w" "wrap    b → (b)"           my/lisp-wrap)]])

;; ---- minor mode with keybindings ----

(defvar-keymap my/lisp-edit-mode-map
  "C-<right>" #'my/lisp-absorb
  "C-<left>"  #'my/lisp-expel
  "M-S"       #'my/lisp-unwrap
  "M-("       #'my/lisp-wrap
  "C-c x"     #'my/lisp-menu)

(define-minor-mode my/lisp-edit-mode
  "Structural editing for Lisp-family modes."
  :lighter nil
  :keymap my/lisp-edit-mode-map)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                scheme-ts-mode-hook))
  (add-hook hook #'my/lisp-edit-mode))

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
;;;; ---- Mode Line ----
;;; =========================================================================
;; Compact mode line showing column number, buffer size, and current
;; function name (useful for navigating large files).

(setq mode-line-compact 'long)
(column-number-mode 1)
(size-indication-mode 1)
(which-function-mode 1)
;; Show empty string instead of "???" when outside a function,
;; so the mode line doesn't change width between function/top-level.
(setq which-func-unknown "")

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
;;;; ---- Casual Suite ----
;;; =========================================================================
;; Transient menus for discoverable access to dired, ibuffer, calc,
;; info, bookmarks, org-agenda, and re-builder commands.
;; C-o opens the context-appropriate transient menu in each mode.

(use-package casual-suite
  :demand t
  :config
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g a" #'casual-avy-tmenu))

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

(load (expand-file-name "modules/search" user-emacs-directory))

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
