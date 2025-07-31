;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:


;; GARBAGE COLLECTION
;; Garbage collection significantly affects startup times.
;; This setting disables GC during startup and resets it after.

(setq gc-cons-threshold most-positive-fixnum)
(let ((threshold (* 32 1024 1024)))
  (defun restore-gc-cons-threshold ()
    "Restore `minimal-emacs-gc-cons-threshold'."
    (setq gc-cons-threshold threshold))
  (add-hook 'emacs-startup-hook #'restore-gc-cons-threshold 105))


;; SECURITY


(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption


;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(setq use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ask the user whether to terminate asynchronous compilations on exit.
;; This prevents native compilation from leaving temporary files in /tmp.
(setq native-comp-async-query-on-exit t)
(setq native-comp-async-report-warnings-errors nil)

(use-package files
  :ensure nil
  :config
  ;; Delete by moving to trash in interactive mode
  (setq delete-by-moving-to-trash (not noninteractive))
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)

  ;; Ignoring this is acceptable since it will redirect to the buffer regardless.
  (setq find-file-suppress-same-file-warnings t)

  ;; Resolve symlinks so that operations are conducted from the file's directory
  (setq find-file-visit-truename t)

  ;; According to the POSIX, a line is defined as "a sequence of zero or more
  ;; non-newline characters followed by a terminating newline".
  (setq require-final-newline t))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;; Process
;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

;;; Performance: Miscellaneous options

(when (and (not (daemonp)) (not noninteractive))
  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode'
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))
;;; Cursor

;;; Text editing, indent, font, and formatting

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;; A longer delay can be annoying as it causes a noticeable pause after each
;; deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)


;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;;; Misc

(setq whitespace-line-column nil)  ; whitespace-mode

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq tramp-verbose 1)
(setq tramp-completion-reread-directory-timeout 50)
(setq remote-file-name-inhibit-cache 50)

;;; Remove warnings from narrow-to-region, upcase-region...
(dolist (cmd '(list-timers narrow-to-region upcase-region downcase-region
                           erase-buffer scroll-left dired-find-alternate-file))
  (put cmd 'disabled nil))

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)
(setq sh-indent-after-continuation 'always)

;; ls-lisp
(setq ls-lisp-verbosity nil)
(setq ls-lisp-dirs-first t)

;; TODO - check if the stuff below is needed

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(global-hl-line-mode)

;; INFO - enable for debugging
(setq debug-on-error nil)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 30)

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-package)
(require 'init-base)
(require 'init-modeline)
(require 'init-ui)

(require 'init-window)
(require 'init-search)
(require 'init-dired)
(require 'init-git)
(require 'init-session)
(require 'init-edit)
(require 'init-ibuffer)
(require 'init-icons)

(require 'init-prog)
(require 'init-text)
(require 'init-org)
(require 'init-scheme)
(require 'init-shell)
(require 'init-markdown)
(require 'init-protobuf)
(require 'init-cmake)
(require 'init-cpp)
(require 'init-yaml)
(require 'init-xml)
(require 'init-rust)
(require 'init-asm)
(require 'init-fsharp)
(require 'init-python)
(require 'init-r)
(require 'init-reader)
(require 'init-docker)
(require 'init-proc)
(require 'init-misc)
(require 'init-help)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(anzu avy-zap beginend calibre calibredb cape casual-suite clang-format
          cmake-mode compile-multi-embark compile-multi-nerd-icons
          compiler-explorer consult-compile-multi consult-dir
          consult-eglot-embark consult-flycheck consult-git-log-grep
          consult-ls-git consult-project-extra consult-todo corfu dape dashboard
          deadgrep devdocs diff-hl dired-git dired-git-info dired-quick-sort
          dired-rsync diredfl docker doom-modeline drag-stuff easysession
          eglot-fsharp eldoc-box eldoc-cmake ess exec-path-from-shell
          expand-region fancy-compilation fd-dired flycheck-eglot flycheck-rust
          format-all fzf geiser-chez git-modes goto-chg helpful htmlize
          hungry-delete ialign ibuffer-project indent-bars jinx lispy magit
          major-mode-hydra marginalia minions mistty multiple-cursors mwim
          nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer nov orderless
          org-modern org-present page-break-lines pdf-tools popper protobuf-mode
          quickrun riscv-mode rustic saveplace-pdf-view sudo-edit
          tempel-collection tree-sitter-langs treesit-fold undo-fu
          undo-fu-session vertico vundo yaml-mode))
 '(safe-local-variable-values
   '((compile-multi-config
      (t ("cmake:configure" . "cmake -S ./ -B ./build")
         ("cmake:build" . "cmake --build ./build")
         ("cmake:clear" . "cmake --build ./build --clear"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
