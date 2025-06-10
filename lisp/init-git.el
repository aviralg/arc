;; init-git.el --- Initialize Git -*- lexical-binding: t -*-

;;; VC

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files

;; Resolve symlinks so that operations are conducted from the file's directory
(setq vc-follow-symlinks t)

(use-package transient
  :ensure t)

;; https://magit.vc/manual/magit
;; TODO
(use-package magit
  :ensure t)

;; https://github.com/dandavison/magit-delta
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

;; https://magit.vc/manual/forge
;; TODO - setup
(use-package forge
  :ensure t
  :after magit)

;; https://github.com/Ailrun/magit-lfs
(use-package magit-lfs
  :ensure t
  :after magit)

;; https://github.com/emacsorphanage/git-messenger
(use-package git-messenger
  :ensure t
  :config
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
  (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)
  ;; Use magit-show-commit for showing status/diff commands
  (custom-set-variables '(git-messenger:use-magit-popup t)))

;; https://codeberg.org/pidu/git-timemachine
;; TODO - setup
(use-package git-timemachine
  :ensure t)

;; https://github.com/jwiegley/git-undo-el
;; TODO - install from github
;; (use-package git-undo
;;   :ensure t)

;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :ensure t
  :bind (( "C-c g g" . browse-at-remote))
  :config
  (setq browse-at-remote-add-line-number-if-no-region-selected nil))

;; https://github.com/alphapapa/magit-todos
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

;; https://github.com/magit/git-modes
(use-package git-modes
  :after (magit)
  :ensure t)


;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L210-L219
;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(provide 'init-git)
