;; init-edit.el --- Editing utilities -*- lexical-binding: t -*-

;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality.
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; The undo-fu-session package complements undo-fu by enabling the saving
;; and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode))

;; Treat undo history as a tree
;; https://github.com/casouri/vundo
(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

(setq kill-ring-max 200)

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :config
  ;; case sensitive makes selection easier
  (setq avy-case-fold-search nil))

;; https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :ensure t
  :config
  (global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
  (global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim))

;; https://github.com/wolray/symbol-overlay
;; TODO - configure properly
(use-package symbol-overlay
  :ensure t
  :bind
  (("M-i" . symbol-overlay-put)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)
   ("<f7>" . symbol-overlay-mode)
   ("<f8>" . symbol-overlay-remove-all)))

;; https://github.com/magnars/expand-region.el
;; TODO - consider expreg or combobulate after building treesitter support
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Hungry deletion
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish
  :hook (prog-mode . turn-on-hungry-delete-mode))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Hanlde minified code
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Goto last change
;; https://github.com/emacs-evil/goto-chg
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Redefine M-< and M-> for some modes
;; https://github.com/DamienCassou/beginend
(use-package beginend
  :ensure t
  :demand t
  :config
  (beginend-global-mode))

;; Drag stuff (lines, words, region, etc...) around
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
              ("<C-return>" . rect-hydra/body)
              :map prog-mode-map
              ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title "Rectangle" :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c m" . multiple-cursors-hydra/body)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space))
  :pretty-hydra
  ((:title "Multiple Cursors" :color amaranth :quit-key ("q" "C-g"))
   ("Up"
    (("p" mc/mark-previous-like-this "prev")
     ("P" mc/skip-to-previous-like-this "skip")
     ("M-p" mc/unmark-previous-like-this "unmark")
     ("|" mc/vertical-align "align with input CHAR"))
    "Down"
    (("n" mc/mark-next-like-this "next")
     ("N" mc/skip-to-next-like-this "skip")
     ("M-n" mc/unmark-next-like-this "unmark"))
    "Misc"
    (("l" mc/edit-lines "edit lines" :exit t)
     ("a" mc/mark-all-like-this "mark all" :exit t)
     ("s" mc/mark-all-in-region-regexp "search" :exit t)
     ("<mouse-1>" mc/add-cursor-on-click "click"))
    "% 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")"
    (("0" mc/insert-numbers "insert numbers" :exit t)
     ("A" mc/insert-letters "insert letters" :exit t)))))

(provide 'init-edit)
