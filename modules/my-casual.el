;;; modules/my-casual.el --- Transient menus -*- lexical-binding: t; -*-

;; Transient menus for discoverable access to dired, ibuffer, calc,
;; info, bookmarks, org-agenda, and re-builder commands.
;; C-o opens the context-appropriate transient menu in each mode.
;; This overrides dired-display-file in dired and similar mode-specific
;; C-o bindings; those commands are accessible inside the transient menu.
(use-package casual-suite
  :ensure t
  :demand t
  :config
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g a" #'casual-avy-tmenu))

(provide 'my-casual)
;;; modules/my-casual.el ends here
