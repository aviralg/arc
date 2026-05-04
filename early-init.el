;;; early-init.el --- Pre-GUI optimizations -*- lexical-binding: t; -*-
;;
;; This file runs before init.el and before the GUI frame is created.
;; It configures performance-critical settings that must be set before
;; Emacs initializes packages or renders the first frame.

;;;; ---- Garbage Collection ----
;; Maximize GC threshold during startup to avoid collection pauses while
;; loading packages. Restored to 64MB in emacs-startup-hook below.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;;;; ---- Package System ----
;; Prevent package.el from auto-initializing before init.el runs.
;; init.el calls package-initialize explicitly after setting archives.
(setq package-enable-at-startup nil)

;;;; ---- Frame & UI Chrome ----
;; Disable UI elements before the frame renders to avoid a flash of
;; toolbar/scrollbar/menubar that immediately gets hidden.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent frame resizing when font face or size changes, and enable
;; pixel-level frame sizing for precise window management.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;;;; ---- File Name Handlers ----
;; Remove all file-name-handler-alist entries during startup. Each entry
;; (TRAMP, compressed files, etc.) is checked on every file operation.
;; Restored after startup, merging any handlers added during init.
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; ---- Post-Startup Restoration ----
;; Restore file handlers and GC threshold after init completes.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups
                   (append file-name-handler-alist
                           my--file-name-handler-alist))
                  gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)
            (makunbound 'my--file-name-handler-alist)))

;;;; ---- Native Compilation ----
;; Silence native-comp warnings that pop up during async compilation.
;; Limit async compilation workers to 1/4 of CPU cores to prevent
;; laptop overheating and system sluggishness.
;; Redirect eln-cache into state/ — must be done here in early-init.el,
;; not init.el, because files may be queued for compilation before init runs.
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t
        native-comp-async-jobs-number
        (max 1 (/ (num-processors) 4)))
  (startup-redirect-eln-cache
   (expand-file-name "state/eln-cache/" user-emacs-directory)))

;;; early-init.el ends here
