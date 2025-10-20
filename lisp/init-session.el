;; init-session.el --- Session management -*- lexical-binding: t -*-


;; BACKUP
;; Avoid generating backups or lockfiles to prevent creating world-readable
;; copies of files.


(use-package emacs
  :ensure nil
  :config
  ;; Avoid backups or lockfiles to prevent creating world-readable copies of files
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq backup-by-copying-when-linked t)
  (setq backup-by-copying t)  ; Backup by copying rather renaming
  (setq delete-old-versions t)  ; Delete excess backup versions silently
  (setq version-control t)  ; Use version numbers for backup files
  (setq kept-new-versions 5)
  (setq kept-old-versions 5))


;; RECENTF
;; recentf is an Emacs package that maintains a list of recently accessed
;; files, making it easier to reopen files you have worked on recently.

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 300)    ; default is 20
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'mode)
  ;; Update recentf-exclude
  (setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (add-hook 'after-init-hook #'(lambda()
                                 (let ((inhibit-message t))
                                   (recentf-mode 1))))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))


;; SAVEPLACE
;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.

(use-package saveplace
  :ensure t
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq save-place-limit 600)
  (add-hook 'after-init-hook #'save-place-mode))


;; SAVEHIST
;; savehist preserves the minibuffer history between sessions.
;; It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.

(use-package savehist
  :ensure nil
  :config
  (setq history-length 300)
  (setq savehist-save-minibuffer-history t)  ;; Default
  (setq savehist-additional-variables
        '(kill-ring                        ; clipboard
          register-alist                   ; macros
          mark-ring global-mark-ring       ; marks
          search-ring regexp-search-ring)) ; searches
  (add-hook 'after-init-hook #'savehist-mode))



;; AUTOSAVE
;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.


(use-package emacs
  :ensure nil
  :config
  (setq auto-save-default t)

  (setq auto-save-interval 300)
  (setq auto-save-timeout 30)
  (setq auto-save-no-message t)

  ;; Do not auto-disable auto-save after deleting large chunks of text.
  (setq auto-save-include-big-deletions t)

  (setq auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory))
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))

  ;; Auto save options
  (setq kill-buffer-delete-auto-save-files t)

  ;; Remove duplicates from the kill ring to reduce clutter
  (setq kill-do-not-save-duplicates t))


;; AUTO-SAVE-VISITED
;; When auto-save-visited-mode is enabled, Emacs will
;; auto-save file-visiting buffers after a certain amount of idle time if the
;; user forgets to save it with save-buffer or C-x s for example.  This is
;; different from auto-save-mode: auto-save-mode periodically saves all modified
;; buffers, creating backup files, including those not associated with a file,
;; while auto-save-visited-mode only saves file-visiting buffers after a period
;; of idle time, directly saving to the file itself without creating backup
;; files.


(use-package emacs
  :ensure nil
  :config
  (setq auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
  (auto-save-visited-mode 1))


;; AUTO-REVERT
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.


(use-package emacs
  :ensure nil
  :config
  (setq revert-without-query (list ".") ; Do not prompt
        auto-revert-stop-on-user-input nil
        auto-revert-verbose t)
  ;; Revert other buffers (e.g, Dired)
  (setq global-auto-revert-non-file-buffers t)

  ;; Resolve issue #29
  (setq global-auto-revert-ignore-modes '(Buffer-menu-mode))

  (add-hook 'after-init-hook #'global-auto-revert-mode))


;; SESSION


(use-package easysession
  :ensure t
  :custom
  (easysession-mode-line-misc-info-format '(" {" easysession-mode-line-session-name "} "))
  (easysession-mode-line-misc-info t)   ; Display the session in the modeline
  (easysession-save-interval (* 10 60)) ; Save every 10 minutes

  :init
  ;; Key mappings:
  ;; C-c l for switching sessions
  ;; and C-c s for saving the current session
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save-as)

  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))

(provide 'init-session)
