;; init-prog.el --- Initialize Programming Modes -*- lexical-binding: t -*-


;; ENCODING


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/International.html
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
(set-default-coding-systems 'utf-8)

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)


;; COMMENTS


;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)


;; INDENTATION


;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent nil)

;; Insert a newline, then indent according to the major mode when ~RETURN~
;; is pressed. This results in code that is indented throughout
;; construction.
;; (define-key global-map (kbd "RET") 'newline-and-indent)


;; COMPILATION

(use-package compile
  :ensure t
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)

  ;; Recenter to the middle of the window for `compile-goto-error', which is also
  ;; used by `wgrep' and `embark-export'.
  (setq next-error-recenter '(4)))

;;; hl-line-mode
(use-package hl-line
  :ensure nil
  :config
  ;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
  ;; clutter and slightly improving `hl-line-mode' performance.
  (setq hl-line-sticky-flag nil)
  (setq global-hl-line-sticky-flag nil)
  (global-hl-line-mode))

;; https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :ensure t
  :hook ((prog-mode) . indent-bars-mode)
  :config
  (setq indent-bars-pattern "."
        indent-bars-width-frac 0.5
        indent-bars-pad-frac 0.25
        indent-bars-display-on-blank-lines nil))

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;  https://github.com/minad/consult-flycheck
(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

;; TODO - uncommenting causes crashes
;; (use-package flycheck-eglot
;;   :ensure t
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHITESPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
(use-package whitespace
  :config
  ;; Commented since there are too many 'valid' whitespaces in some modes.
  ;; (setq-default show-trailing-whitespace t)
  (setq whitespace-style '(face tabs trailing))
  (set-face-attribute 'whitespace-tab nil
                      :background "red"
                      :foreground "yellow"
                      :weight 'bold)
  (add-hook 'prog-mode-hook 'whitespace-mode)
  ;; Delete trailing tabs and spaces on save of a file.
  (add-hook 'before-save-hook 'whitespace-cleanup)
  )

;; Spaces instead of tabs when indenting.
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XREF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cross-referencing commands
(use-package xref
  :init
  ;; Use faster search tool
  (setq xref-search-program 'ripgrep)

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EGLOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t)

  ;; Activate Eglot in cross-referenced non-project files
  (setq eglot-extend-to-xref t)

  ;; Eglot optimization
  (setq eglot-report-progress nil)      ; Prevent Eglot minibuffer spam

  ;; TODO - check
  ;; (setq read-process-output-max (* 1024 1024) ;1 MB
  ;;       eglot-send-changes-idle-time 0.5))
  )

;; https://github.com/flycheck/flycheck-eglot
;; (use-package flycheck-eglot
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

(use-package eldoc-box
  :ensure t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEVDOCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse devdocs.io documents using EWW
(use-package devdocs
  :ensure t
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
              ("M-<f1>" . devdocs-dwim)
              ("C-h D"  . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))

      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(defun project--root ()
  "Get root of current project."
  (project-root (project-current)))

;; https://github.com/mohkale/compile-multi
(use-package compile-multi
  :ensure t
  :config
  (setq compile-multi-default-directory #'project--root)
  (setq compile-multi-config nil))

(use-package consult-compile-multi
  :ensure t
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :ensure t
  :after nerd-icons-completion
  :after compile-multi)

(use-package compile-multi-embark
  :ensure t
  :after (compile-multi embark)
  :demand t
  :config (compile-multi-embark-mode +1))

(use-package fancy-compilation
  :ensure t
  :config
  (fancy-compilation-mode)
  (setq fancy-compilation-term "eterm-color")
  (setq fancy-compilation-override-colors nil)
  (setq fancy-compilation-quiet-prelude nil)
  (setq fancy-compilation-quiet-prolog nil))

;; https://github.com/emacsorphanage/quickrun
(use-package quickrun
  :ensure t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

(use-package dape
  :ensure t
  :config

  ;; Add hints for variable values in buffer
  (setq dape-inlay-hints t
        dape-buffer-window-arrangement 'right)

  ;; Emphasize currently source line of current debugged program
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Create a memorable alias for `dape'.
  (defalias 'start-debugging #'dape))


;; SNIPPETS

;;; abbrev

;; Ensure `abbrev_defs` is stored in the correct location when
;; `user-emacs-directory` is modified, as it defaults to ~/.emacs.d/abbrev_defs
;; regardless of the change.
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(setq save-abbrevs 'silently)

;;; dabbrev

(setq dabbrev-upcase-means-case-search t)

;;; https://github.com/minad/tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;; https://github.com/Crandel/tempel-collection
(use-package tempel-collection
  :ensure t
  :after tempel)


;; HIGHLIGHT


;; Highlight TODO and friends.
(use-package hl-todo
  :ensure t
  :demand t
  :config
  (setq hl-todo-keyword-faces '(("TODO"   . "#FF0000")
                                ("FIXME"  . "#00AA00")
                                ("DEBUG"  . "#A020F0")
                                ("STUB"   . "#1E90FF")))
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert)

  (global-hl-todo-mode t))

(provide 'init-prog)
