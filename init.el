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
;; Configure ELPA/MELPA archives. Each use-package block declares
;; :ensure t to install its own package on first use.

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

;; use-package :ensure t handles installation — prevent customize
;; from maintaining a redundant package-selected-packages list.
(setq package-selected-packages nil)

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
  :ensure t
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
  :ensure t
  :demand t
  :if (memq window-system '(mac ns))
  :config
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

(add-to-list 'load-path
             (expand-file-name "modules/" user-emacs-directory))

(defun my--load-module (name)
  "Require the module my-NAME from the modules/ directory.
Errors are caught and displayed in *Warnings* so one broken module
doesn't prevent the rest from loading."
  (condition-case err
      (require (intern name))
    (error (display-warning 'init (format "Module %s failed: %s" name err) :error))))

;; Load order matters:
;;   defaults  — core settings used by everything
;;   buffers   — savehist must load before completion (corfu-history)
;;   completion — consult/embark must load before search, code, navigation
;;              (named my-completion to avoid shadowing built-in)
;;   editing   — standalone
;;   theme     — standalone
;;   windows   — standalone
;;   search    — uses isearch (consult-line binding in isearch-mode-map)
;;   code      — uses consult (consult-flymake, consult-xref, consult-eglot)
;;   git       — standalone
;;   dired     — standalone (named my-dired to avoid shadowing built-in)
;;   org       — standalone (named my-org to avoid shadowing built-in)
;;   shell     — uses consult (consult-history in eshell)
;;              (named my-shell to avoid shadowing built-in)
;;   spelling  — ispell must load before flyspell
;;   navigation — uses embark (avy-action-embark)
;;   casual    — uses keymaps from dired, ibuffer, org, etc.
;;   languages — uses eglot (pet-eglot-setup, eglot-ensure)

(my--load-module "my-defaults")
(my--load-module "my-buffers")
(my--load-module "my-completion")
(my--load-module "my-editing")
(my--load-module "my-theme")
(my--load-module "my-windows")
(my--load-module "my-search")
(my--load-module "my-code")
(my--load-module "my-git")
(my--load-module "my-dired")
(my--load-module "my-org")
(my--load-module "my-shell")
(my--load-module "my-spelling")
(my--load-module "my-navigation")
(my--load-module "my-casual")
(my--load-module "my-languages")
(my--load-module "my-cheatsheet")
(my--load-module "my-cheatsheet-entries")

;;; =========================================================================
;;;; ---- Local Overrides ----
;;; =========================================================================
;; Load optional, untracked files for machine-specific or private settings.
;; These are gitignored and not part of the committed config.

(let ((file (locate-user-emacs-file "compile-commands.el")))
  (when (file-exists-p file)
    (load file)))

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
