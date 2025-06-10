;; init-org.el --- Initialize Org Mode -*- lexical-binding: t -*-
;;;  TODO
;; https://github.com/emacs-compat/compat/tree/main
;; INFO: Needed for org-timeblock
;;       cannot install, find the package repos configured
;; (use-package compat
;;   :ensure t)
;;
;; ;; https://github.com/ichernyshovvv/org-timeblock
;; (use-package org-timeblock
;;   :after (org compat)
;;   :ensure t)

;; https://github.com/fniessen/org-html-themes
;; INFO: how to install?
;; (use-package org-html-themes
;;   :after org
;;  :ensure t)

;; https://github.com/minad/org-modern
;; TODO: changes config for all files
;; (use-package org-modern
;;   :ensure t
;;   :after org
;;   :config
;;   ;; Add frame borders and window dividers
;;   (modify-all-frames-parameters
;;    '((right-divider-width . 40)
;;      (internal-border-width . 40)))
;;   (dolist (face '(window-divider
;;                   window-divider-first-pixel
;;                   window-divider-last-pixel))
;;     (face-spec-reset-face face)
;;     (set-face-foreground face (face-attribute 'default :background)))
;;   (set-face-background 'fringe (face-attribute 'default :background))
;;
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t
;;
;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-agenda-tags-column 0
;;    org-ellipsis "…")
;;   (global-org-modern-mode))


(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t))

;; https://github.com/snosov1/toc-org
;; (use-package toc-org
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook 'toc-org-mode)
;;
;;   ;; enable in markdown, too
;;   (add-hook 'markdown-mode-hook 'toc-org-mode)
;;   (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

;; https://github.com/rlister/org-present
(use-package org-present
  :ensure t)

;; https://github.com/minad/org-modern
(use-package org-modern
  :ensure t
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode))

(provide 'init-org)
