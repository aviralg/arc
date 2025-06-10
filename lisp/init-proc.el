;; init-proc.el --- Initialize Process Handling -*- lexical-binding: t -*-

;; https://laurencewarne.github.io/emacs/programming/2022/12/26/exploring-proced.html
;; TODO - analyze the format and find why all fields are not showing up
(use-package proced
  :ensure t
  :commands proced
  :bind (("C-M-p" . proced))
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'verbose))

(provide 'init-proc)
;;; init-proc.el ends here
