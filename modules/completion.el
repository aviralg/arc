;;; modules/completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

;;; --- Minibuffer Completion ---
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
  :after savehist
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

;;; modules/completion.el ends here
