;; init-search.el --- Initialize Find and Replace -*- lexical-binding: t -*-

(use-package isearch
  :ensure nil
  :config
  ;; Eliminate delay before highlighting search matches
  (setq lazy-highlight-initial-delay 0))

(use-package grep
  :ensure nil
  :init
  (grep-apply-setting 'grep-command "rg --color=auto --null -nH --no-heading -e ")
  (grep-apply-setting 'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
  (grep-apply-setting 'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
  (grep-apply-setting 'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))

;; TODO - deadgrep and wgrep-deadgrep
;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "<f5>") #'deadgrep))

;; https://github.com/mhayashi1120/Emacs-wgrep
;; (use-package wgrep
;;   :ensure t
;;   :config
;;   ;; https://github.com/mhayashi1120/Emacs-wgrep
;;   ;; https://github.com/mhayashi1120/Emacs-wgrep/blob/master/wgrep-deadgrep.el#L41-L42
;;   (autoload 'wgrep-deadgrep-setup "wgrep-deadgrep")
;;   (add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup))

;; https://github.com/bling/fzf.el
(use-package fzf
  :ensure t
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; Show number of matches in mode-line while searching
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :ensure t
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

;; Visual `align-regexp'
;; https://github.com/mkcms/interactive-align
(use-package ialign
  :ensure t)

(provide 'init-search)
