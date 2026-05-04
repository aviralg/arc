;;; modules/my-languages.el --- Programming language modes -*- lexical-binding: t; -*-

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
  :ensure t
  :demand t
  :config
  (defun my--pet-setup-python ()
    (when-let* ((python (pet-executable-find "python")))
      (setq-local python-shell-interpreter python))
    (when-let* ((root (pet-virtualenv-root)))
      (setq-local python-shell-virtualenv-root root))
    (pet-eglot-setup)
    (eglot-ensure))
  (add-hook 'python-ts-mode-hook #'my--pet-setup-python))

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
  :ensure t
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
  :ensure t
  :demand t
  :mode "\\.swift\\'")

;;; --- Lisp structural editing ---
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
;;   M-(         wrap  (shadows insert-parentheses — use C-u N M-( via
;;                      M-x insert-parentheses if you need to wrap N sexps)
;;
;; Limitation: these commands operate on sexps as Emacs sees them.
;; Comments between sexps may be included in the gap text during
;; absorb/expel since skip-chars-forward doesn't skip comments.

;; ---- helpers ----

(defun my--lisp-enclosing-open ()
  "Return the position of the opening delimiter of the enclosing list.
Signals `user-error' if point is not inside any list."
  (condition-case nil
      (save-excursion
        (backward-up-list 1 t t)
        (point))
    (scan-error
     (user-error "Not inside any list"))))

(defun my--lisp-enclosing-close ()
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

  (a b c) d  →  (a b c d)

Comments between the closing delimiter and the next sexp are
skipped (left outside the list)."
  (interactive)
  (let* ((close-pos   (my--lisp-enclosing-close))
         (close-char  (char-after close-pos))
         (after-close (1+ close-pos))
         (next-end
          (condition-case nil
              (save-excursion
                (goto-char after-close)
                (forward-comment (buffer-size))
                (forward-sexp 1)
                (point))
            (scan-error
             (user-error "absorb: no next sibling sexp")))))
    (let ((gap-and-sexp (delete-and-extract-region after-close next-end)))
      (delete-region close-pos (1+ close-pos))
      (goto-char close-pos)
      (insert gap-and-sexp)
      (insert close-char)
      (indent-region (my--lisp-enclosing-open) (1+ (point))))))

;; ---- expel (barf forward) ----

(defun my/lisp-expel ()
  "Push the last sexp out of the current list.

  (a b c d)  →  (a b c) d

Comments between the last sexp and the closing delimiter are
skipped (left inside the list)."
  (interactive)
  (let* ((close-pos  (my--lisp-enclosing-close))
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
                      (forward-comment (- (buffer-size)))
                      (point)))
          (sexp-text (buffer-substring-no-properties
                      last-sexp-start last-sexp-end)))
      (delete-region ws-start (1+ close-pos))
      (goto-char ws-start)
      (insert close-char " " sexp-text)
      (indent-region (my--lisp-enclosing-open) (point)))))

;; ---- unwrap (splice) ----

(defun my/lisp-unwrap ()
  "Remove the delimiters of the enclosing list, splicing its contents.

  (a (b c) d) with point in (b c)  →  (a b c d)"
  (interactive)
  (let ((open-pos  (my--lisp-enclosing-open))
        (close-pos (my--lisp-enclosing-close)))
    (delete-region close-pos (1+ close-pos))
    (delete-region open-pos  (1+ open-pos))))

;; ---- wrap ----

(defun my/lisp-wrap ()
  "Surround the sexp at point, or the active region, with parentheses.

  point before b in `a b c'  →  `a (b) c'
  region `b c'               →  `(b c)'

Point is placed just inside the opening paren after the operation.
Unlike the built-in `insert-parentheses', this does not accept a
prefix argument to wrap N sexps.  Use M-x insert-parentheses for that."
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
                scheme-mode-hook))
  (add-hook hook #'my/lisp-edit-mode))

(provide 'my-languages)
;;; modules/my-languages.el ends here
