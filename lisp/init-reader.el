;; init-pdf.el --- Initialize PDF Modes -*- lexical-binding: t -*-

;; (use-package pdf-tools
;;   :ensure t)

;; (use-package pdf-view-restore
;;   :after pdf-tools
;;   :config
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
;;   :custom
;;   (pdf-view-restore-filename
;;    (minimal-emacs-load-user-init "var/pdf-view-restore")))

(use-package pdf-tools
  :ensure t
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init (setq pdf-view-use-scaling t
              pdf-view-use-imagemagick nil
              pdf-annot-activate-created-annotations t)
  :config
  ;; Activate the package
  (pdf-tools-install t nil t nil))

;; Recover last viewed position
(use-package saveplace-pdf-view
  :ensure t
  :when (ignore-errors (pdf-info-check-epdfinfo) t)
  :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
  :init
  (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
  (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/Ebooks")
  ;; for folder driver metadata: it should be .metadata.calibre
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-search-page-max-rows 100)
  (setq calibredb-id-width 4)
  (setq calibredb-size-show t)
  (setq calibredb-format-nerd-icons t)
  (setq calibredb-format-all-the-icons t)
  (setq calibredb-format-icons-in-terminal t)
  (setq calibredb-format-character-icons t))

;; Another Atom/RSS reader
(use-package newsticker
  :ensure nil
  :bind ("C-x W" . newsticker-show-news)
  :init (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
                ("Oremacs" "https://oremacs.com/atom.xml")
                ("EmacsCast" "https://pinecast.com/feed/emacscast")
                ("Emacs TIL" "https://emacstil.com/feed.xml")
                ;; ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")
                )))


(provide 'init-reader)
