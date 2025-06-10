;; -*- lexical-binding: t; -*-

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;; Makes Emacs omit the load average information from the mode line.
(setq display-time-default-load-average nil)

(use-package minions
  :ensure t
  :config
  (minions-mode))

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :after (minions)
  :config
  (doom-modeline-mode 1)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud nil)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)

  ;; (setq doom-modeline-project-name t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback t)

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info t)

  ;; Whether display the total line number
  (setq doom-modeline-total-line-number t)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 30)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github t)

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus nil)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc nil)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery nil)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time nil)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version nil)

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  (setq doom-modeline-always-visible-segments nil))

(provide 'init-modeline)
