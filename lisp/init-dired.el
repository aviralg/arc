;; init-dired.el --- Initialize dired -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  ;; TODO: do we need this?
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)


;;; Dired

  (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
  (setq dired-free-space nil
        dired-dwim-target t ; Propose a target for intelligent moving or copying.
        dired-deletion-confirmer 'y-or-n-p
        dired-filter-verbose nil
        dired-recursive-deletes 'top
        dired-recursive-copies 'always
        dired-create-destination-dirs 'ask
        image-dired-thumb-size 150)

  (setq dired-vc-rename-file t)

  ;; Disable the prompt about killing the Dired buffer for a deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; dired-omit-mode
  (setq dired-omit-verbose nil)
  (setq dired-omit-files (concat "\\`[.]\\'"))
  )

(use-package dired-aux
  :ensure nil)

;; TODO - copied from Centaur Emacs. Simplify!
(use-package dired-x
  :ensure nil
  :demand t
  :config
  (let ((cmd (cond (sys/mac-x-p "open")
                   (sys/linux-x-p "xdg-open")
                   (sys/win32p "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))


(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; https://gitlab.com/xuhdev/dired-quick-sort
(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

;; https://github.com/purcell/diredfl
(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

;; (use-package dired-git
;;   :ensure t
;;   :config
;;   (add-hook 'dired-mode-hook 'dired-git-mode))

;; Show git info in dired
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package fd-dired
  :ensure t)

;; Allow rsync from dired buffers
(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(provide 'init-dired)
