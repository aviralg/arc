;;; modules/dired.el --- File management -*- lexical-binding: t; -*-

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

;;; modules/dired.el ends here
