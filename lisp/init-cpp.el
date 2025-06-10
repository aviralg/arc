;; init-cpp.el --- Initialize C/C++ -*- lexical-binding: t -*-

(use-package clang-format
  :ensure t)

(use-package cc-mode
  :ensure nil
  :after clang-format
  :bind (:map c-mode-base-map
              ("<f12>" . compile)
              ("C-M-\\" . clang-format-region)
              ("C-i" . clang-format))
  :init
  (setq c-basic-offset 4
        c-basic-indent 4)
  :config
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c-or-c++-mode-hook 'eglot-ensure)

  (fset 'c-indent-region 'clang-format-region)
  (fset 'indent-line-function 'clang-format-region)

  ;; emacs-fu: don’t indent inside of C++ namespaces
  ;; http://brrian.tumblr.com/post/9018043954/emacs-fu-dont-indent-inside-of-c-namespaces
  (c-set-offset 'innamespace 0)

  ;; .h files to open in c++-mode rather than c-mode.
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode)))

(major-mode-hydra-define c++-mode nil
  ("Code Actions"
   (("i" eglot-code-action-inline "inline")
    ("e" eglot-code-action-extract "extract")
    ("r" eglot-code-action-rewrite "rewrite")
    ("R" eglot-rename "rename")
    ("q" eglot-code-action-quickfix "quickfix")
    ("o" eglot-code-action-organize-imports "organize imports"))
   "Format"
   (("b" eglot-format-buffer "buffer")
    ("=" eglot-format "region"))
   "Analysis"
   (("." xref-find-definitions "definition")
    ("?" xref-find-references "references")
    ("t" eglot-show-type-hierarchy "type hierarchy")
    ("c" eglot-show-call-hierarchy "call hierarchy")
    ("h" eglot-inlay-hints-mode "hints")
    ("H" eldoc "eldoc"))))

;; https://github.com/mkcms/compiler-explorer.el
(use-package compiler-explorer
  :ensure t)

(provide 'init-cpp)
