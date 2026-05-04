;;; modules/my-spelling.el --- Spell checking -*- lexical-binding: t; -*-

;; Use hunspell as the spell-check backend when available.
;; On macOS, brew install hunspell installs the binary but NOT dictionaries.
;; Download en_US dictionary files manually:
;;   mkdir -p ~/Library/Spelling
;;   curl -o ~/Library/Spelling/en_US.aff https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
;;   curl -o ~/Library/Spelling/en_US.dic https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
(use-package ispell
  :ensure nil
  :demand t
  :config
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US")))

;; Auto-enable on-the-fly spell checking in text buffers and
;; comments/strings in code buffers. Only if a spell checker is installed.
(use-package flyspell
  :ensure nil
  :demand t
  :after ispell
  :config
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(provide 'my-spelling)
;;; modules/my-spelling.el ends here
