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
;;;; ---- Modules ----
;;; =========================================================================

(defun my--load-module (name)
  "Load NAME from the modules/ directory."
  (load (expand-file-name (concat "modules/" name) user-emacs-directory)))

(my--load-module "defaults")
(my--load-module "buffers")
(my--load-module "completion")
(my--load-module "editing")
(my--load-module "theme")
(my--load-module "windows")
(my--load-module "search")
(my--load-module "code")
(my--load-module "git")
(my--load-module "dired")
(my--load-module "org")
(my--load-module "shell")
(my--load-module "spelling")
(my--load-module "navigation")
(my--load-module "casual")
(my--load-module "languages")
(my--load-module "cheatsheet")
(my--load-module "cheatsheet-entries")

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
