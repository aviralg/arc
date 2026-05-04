;;; modules/my-code.el --- LSP, tree-sitter, diagnostics, project -*- lexical-binding: t; -*-

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
        eglot-sync-connect nil))

;; Search LSP workspace symbols through consult's interface (M-g s).
(use-package consult-eglot
  :ensure t
  :after (eglot consult)
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
           (when-let* ((lang
                        (pcase (cdr entry)
                          ('python-ts-mode     'python)
                          ('c-ts-mode          'c)
                          ('c++-ts-mode        'cpp)
                          ('c-or-c++-ts-mode   (and (treesit-language-available-p 'c)
                                                    (treesit-language-available-p 'cpp)
                                                    'cpp))
                          ('js-json-ts-mode    'json)
                          ('json-ts-mode       'json)
                          ('yaml-ts-mode       'yaml)
                          ('bash-ts-mode       'bash)
                          ('cmake-ts-mode      'cmake)
                          ('toml-ts-mode       'toml))))
             (treesit-language-available-p lang)))
       '((python-mode    . python-ts-mode)
         (c-mode         . c-ts-mode)
         (c++-mode       . c++-ts-mode)
         (c-or-c++-mode  . c-or-c++-ts-mode)
         ;; Emacs 30 built-in modes — these are the actual mode names
         (js-json-mode   . json-ts-mode)
         (conf-toml-mode . toml-ts-mode)
         (sh-mode        . bash-ts-mode))))

  ;; yaml has no built-in base mode to remap from, so associate file
  ;; extensions directly with the ts-mode. cmake is handled via :mode
  ;; in languages.el.
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

;;; ---- Project Management ----
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
  ;; Uses process-lines to avoid shell metacharacter issues.
  ;; fd -a returns absolute paths, satisfying the project-files API contract.
  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head vc)) &optional dirs)
      (let* ((root (project-root project))
             (default-directory root))
        (if (or (null dirs) (equal dirs (list root)))
            (process-lines "fd" "-t" "f" "-H" "-E" ".git" "-a")
          (cl-call-next-method))))))

(provide 'my-code)
;;; modules/my-code.el ends here
