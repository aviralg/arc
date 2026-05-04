;;; modules/spelling.el --- Spell checking -*- lexical-binding: t; -*-

;; Use hunspell as the spell-check backend when available.
(use-package ispell
  :ensure nil
  :demand t
  :config
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US")))

;; Auto-enable on-the-fly spell checking in text buffers,
;; but only if a spell checker is actually installed.
(use-package flyspell
  :ensure nil
  :demand t
  :config
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)))

;;; modules/spelling.el ends here
