;; init-package.el --- Initialize Package Manager -*- lexical-binding: t -*-

(use-package package
  :ensure nil
  :config
  (setq use-package-compute-statistics t)

  ;; Setting use-package-expand-minimally to (t) results in a more compact output
  ;; that emphasizes performance over clarity.
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t)
  (setq package-enable-at-startup nil)  ; Let the init.el file handle this

  ;; Emacs comes with several built-in packages, such as Org-mode, that are
  ;; essential for many users. However, these built-in packages are often not the
  ;; latest versions available. Ensure that your built-in packages are always up
  ;; to date with:
  (setq package-install-upgrade-built-in t)
  (setq use-package-always-ensure t)

  (setq use-package-enable-imenu-support t)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

  (customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                        ("nongnu" . 80)
                                                        ("melpa"  . 70))))

(provide 'init-package)
