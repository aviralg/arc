;; init-cmake.el --- Initialize CMake -*- lexical-binding: t -*-

(use-package cmake-mode
  :ensure t)

(use-package eldoc-cmake
  :ensure t
  :hook (cmake-mode . eldoc-cmake-enable))

(provide 'init-cmake)
