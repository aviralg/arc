;; init-protobuf.el --- Initialize Protobuf -*- lexical-binding: t -*-

(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'init-protobuf)
