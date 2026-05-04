;;; modules/navigation.el --- Jump and navigate -*- lexical-binding: t; -*-

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

;;; modules/navigation.el ends here
