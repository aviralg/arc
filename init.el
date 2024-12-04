;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Code:


;; CONSTANTS


(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")


;; PACKAGE MANAGEMENT


(use-package package
    :ensure nil
    :init
        (add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; Emacs comes with several built-in packages, such as Org-mode, that are
;; essential for many users. However, these built-in packages are often not the
;; latest versions available. Ensure that your built-in packages are always up
;; to date with:
(setq package-install-upgrade-built-in t)

;; Always ensure packages are installed
(setq use-package-always-ensure t)


;; ENCODING


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/International.html
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
(set-default-coding-systems 'utf-8)

(set-language-environment "UTF-8")


;; COMMENTS


;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)


;; LINE/COLUMN NUMBER


;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)


;; INDENTATION


;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

(setq-default tab-always-indent t)

;; Insert a newline, then indent according to the major mode when ~RETURN~
;; is pressed. This results in code that is indented throughout
;; construction.
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Set the indentation step to 4 for ~CC-mode~ buffers.
(setq-default c-basic-offset 4)


;; CURSOR


;; Change cursor to a non-blinking bar.
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)


;; FRINGE


;; Fringe width
(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)


;; BACKUP

;; Avoid generating backups or lockfiles to prevent creating world-readable
;; copies of files.
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files


;; AUTO SAVE


;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text. The purpose of auto-save is to provide a failsafe, and
;; disabling it contradicts this objective.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)


;; AUTO REVERT


;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)


;; RECENT FILES


;; `recentf' is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-auto-cleanup 'mode)


;; SAVE PLACE


;; `save-place-mode` enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)


;; BELL


;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)


;; WARNING


;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))


;; WORD WRAP


;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)


;; SESSION


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; TODO - this causes problems with writeroom-mode
;; (desktop-save-mode 1)


;; SAVE HISTORY


(savehist-mode)


;; UI


;; Disable various bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Disable GUIs because theyr are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Both initial frame and subsequent frames should be maximized.
;; https://emacs.stackexchange.com/a/3017
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; PATH


;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or (memq window-system '(mac ns x)) (daemonp))
    (exec-path-from-shell-initialize)))


;; THEME


;; https://protesilaos.com/emacs/modus-themes
;; use ~modus-operandi~ theme because it uses soft colors on a white background.
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org#5111-the-prot-emacs-modus-themesel-module
(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-vivendi modus-operandi)
        modus-themes-mode-line '(borderless)

        modus-themes-common-palette-overrides '((bg-mode-line-active bg-blue-intense)
                                                (fg-mode-line-active fg-main)
                                                (border-mode-line-active blue-intense))
        ;; modus-themes-italic-constructs t
        ;; modus-themes-bold-constructs t
        ;; modus-themes-mixed-fonts t
        ;; modus-themes-variable-pitch-ui t

        ;; Make the fringe invisible
        ;; Blue background, neutral foreground, intense blue border for the modeline
        )
  ;; Make the active mode line have a pseudo 3D effect (this assumes
  ;; you are using the default mode line and not an extra package).
  ;; (custom-set-faces '(mode-line ((t :box (:style released-button)))))
  
  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-operandi))


;; FONT


;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; TODO - simplify this
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
;; https://github.com/protesilaos/dotfiles/blob/6570b359d31c5edaca8103fcad57ba7ef8ee85ab/emacs/.emacs.d/prot-emacs-modules/prot-emacs-theme.el#L188
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                        ;; Set last preset or fall back to desired style from `fontaine-presets'.
                        (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind ("C-c f" . fontaine-set-preset)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((38-inch-monitor
           :default-height 180)
          (34-inch-monitor
           :default-height 160)
          (small
           :default-family "Iosevka Comfy Motion"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Duo")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (live-stream
           :default-family "Iosevka Comfy Wide Motion"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "Iosevka Comfy Wide Motion"
           :variable-pitch-family "Iosevka Comfy Wide Duo"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (jumbo
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-slant normal
           :default-height 100

           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-weight bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil))))


;; MODELINE


(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)

  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  ;; TODO - (setq doom-modeline-height 18)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud t)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (setq doom-modeline-window-width-limit 85)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'buffer-name)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerd-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

  ;; When non-nil, mode line displays column numbers zero-based.
  ;; See `column-number-indicator-zero-based'.
  (setq doom-modeline-column-zero-based t)

  ;; Specification of \"percentage offset\" of window through buffer.
  ;; See `mode-line-percent-position'.
  (setq doom-modeline-percent-position '(-3 "%p"))

  ;; Format used to display line numbers in the mode line.
  ;; See `mode-line-position-line-format'.
  (setq doom-modeline-position-line-format '("L%l"))

  ;; Format used to display column numbers in the mode line.
  ;; See `mode-line-position-column-format'.
  (setq doom-modeline-position-column-format '("C%c"))

  ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
  (setq doom-modeline-position-column-line-format '("%l:%c"))

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info t)

  ;; Whether display the total line number
  (setq doom-modeline-total-line-number t)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-check-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 30)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus nil)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc nil)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery nil)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time nil)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version nil)

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  (setq doom-modeline-always-visible-segments nil)

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))


;; DASHBOARD


;;; https://github.com/emacs-dashboard/emacs-dashboard]]
;;; Set a sensible startup screen that displays recently visited files, projects, bookmarks, agenda, and registers.
;;; TODO Study the different configuration variables, and change the logo.
;;; TODO enable projects in dashboard-items
(use-package dashboard
  :ensure t
  :demand t
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title ""
        ;; Set the banner
        dashboard-startup-banner 'logo
        ;; Content is not centered by default. To center, set
        dashboard-center-content t
        ;; vertically center content
        dashboard-vertically-center-content t

        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
        dashboard-items '((recents  . 5)
                          ;;(projects . 5)
                          (bookmarks . 5)
                          (registers . 5)
                          (agenda . 5))
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        ;; display icons on both GUI and terminal
        dashboard-display-icons-p t
        ;; use `nerd-icons' package
        dashboard-icon-type 'nerd-icons 
        ;; TODO: enable this after bug is fixed
        ;; https://github.com/emacs-dashboard/emacs-dashboard/issues/459
        ;;dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-set-footer nil
        dashboard-projects-switch-function 'projectile-persp-switch-project
        ;;dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  (dashboard-setup-startup-hook))


;; MINIBUFFER


(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; savehist
;; `savehist` is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default

;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  ;;(vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))


;; CONSULT


;; https://github.com/minad/consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; https://github.com/karthink/consult-dir
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :ensure t
  :after (consult))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot-embark
  :ensure t
  :after (consult-eglot)
  :init
  (consult-eglot-embark-mode))


;; EMBARK


;; https://github.com/oantolin/embark
(use-package embark
  :ensure t

  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; https://github.com/oantolin/embark
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; ORDERLESS


;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; HYDRA


;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t)

;; https://github.com/jerrypnz/major-mode-hydra.el
(use-package pretty-hydra
  :ensure t)

;; https://github.com/jerrypnz/major-mode-hydra.el
;; TODO - use this to define hydra for all major modes
(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))


;; KEYS

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; ICONS 


;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t)


;; MARGINALIA


;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))


;; CORFU


;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package nerd-icons-corfu
  :ensure t
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; CUT COPY PASTE KILL-RING


;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

(setq kill-ring-max 200)

;; kill & mark things easily
;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; interactively insert and edit items from kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind ("C-c k" . browse-kill-ring)
  :hook (after-init . browse-kill-ring-default-keybindings)
  :init (setq browse-kill-ring-separator "────────────────"
              browse-kill-ring-separator-face 'shadow))

;; https://github.com/rolandwalker/simpleclip
;; ;; https://blog.binchen.org/posts/the-reliable-way-to-access-system-clipboard-from-emacs.html
(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))


;; UNDO REDO


;; https://github.com/casouri/vundo
(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;; EDITING


;; Delete selection if you insert
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L33-L36
(use-package delsel
  :ensure t
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
              ("<C-return>" . rect-hydra/body)
              :map prog-mode-map
              ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title "Rectangle"
           :color amaranth
           :body-pre (rectangle-mark-mode)
           :post (deactivate-mark)
           :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Goto last change
(use-package goto-chg
  :ensure t
  :bind ("C-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L316C1-L323C1
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Hungry deletion
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Handle minified code
;; https://elpa.gnu.org/packages/so-long.html
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq sentence-end-double-space nil)

;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; https://github.com/DamienCassou/beginend
(use-package beginend 
  :ensure t
  :demand t
  :config  
  (beginend-global-mode))

;; https://github.com/duckwork/titlecase.el
(use-package titlecase
  :ensure t
  :after embark
  :config
  (define-key embark-heading-map "T" #'titlecase-line)
  ;; TODO - pick the right one
  ;;(define-key embark-region-map "T" #'titlecase-region)
  )

;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :ensure t
  :config
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle))

;; https://github.com/mkcms/interactive-align
(use-package ialign
  :ensure t
  :config
  (global-set-key (kbd "C-x l") #'ialign))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L221-L225
;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Edit multiple regions in the same way simultaneously
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L230-L242
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Click to browse URL or to send to e-mail address
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L106-L110
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Pass a URL to browser
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L82-L104
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map))
  
  ;; For WSL
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)
      (when (daemonp)
        (advice-add #'browse-url :override #'browse-url-generic)))))

;; emacs-surround
;; https://github.com/ganmacs/emacs-surround
;; TODO - copy el file locally, package not on melpa/elpa
;;  (use-package emacs-surround
;;    :ensure t
;;    :config
;;    (global-set-key (kbd "C-q") 'emacs-surround))

;; https://github.com/magnars/expand-region.el
;; TODO - consider expreg or combobulate after building treesitter support
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-2)  
         ("C-'" . avy-goto-line)
         ("M-g c" . avy-goto-char)
         ("M-g e" . avy-goto-word-0)  ;; lots of candidates
         ("M-g g" . avy-goto-line)    ;; digits behave like goto-line
         ("M-g w" . avy-goto-word-1)  ;; first character of the word
         ("M-g (" . avy-goto-open-paren)
         ("M-g )" . avy-goto-close-paren)
         ("M-g P" . avy-pop-mark))
  :config
  ;; case sensitive makes selection easier
  (setq avy-case-fold-search nil))

;; https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :ensure t
  :config
  (global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
  (global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim))

;; https://github.com/wolray/symbol-overlay
;; TODO - configure properly
(use-package symbol-overlay
  :ensure t
  :bind
  (("M-i" . symbol-overlay-put)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)
   ("<f7>" . symbol-overlay-mode)
   ("<f8>" . symbol-overlay-remove-all)))

;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :ensure t)

;; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Highlight current line.
;; https://emacsredux.com/blog/2013/04/02/highlight-current-line/
(global-hl-line-mode +1)

;; Highlight TODO and friends.
(use-package hl-todo
  :ensure t
  :demand t
  :config
  (setq hl-todo-keyword-faces '(("TODO"   . "#FF0000")
                                ("FIXME"  . "#FF0000")
                                ("DEBUG"  . "#A020F0")
                                ("GOTCHA" . "#FF4500")
                                ("STUB"   . "#1E90FF")))
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert)

  (global-hl-todo-mode t))


;; SEARCH REPLACE


;; TODO - deadgrep and wgrep-deadgrep
;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "<f5>") #'deadgrep))

;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :ensure t
  :config
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  ;; https://github.com/mhayashi1120/Emacs-wgrep/blob/master/wgrep-deadgrep.el#L41-L42
  (autoload 'wgrep-deadgrep-setup "wgrep-deadgrep")
  (add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup))

;; https://github.com/emacsorphanage/anzu
;; Show number of matches in mode-line while searching
;; https://github.com/seagle0128/.emacs.d/blob/ccdde5f9311517b6d7ab9280f24ac5271b095440/lisp/init-edit.el#L183-L190
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))


;; FILE


;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)


;; BUFFER


;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward)


;; WINDOW


;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  :ensure t
  :pretty-hydra
  ((:title "Window Management" :foreign-keys warn :quit-key ("q" "C-g"))
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete")
     ("X" ace-delete-other-windows "delete other" :exit t)
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select" :exit t)
     ("m" toggle-frame-maximized "maximize" :exit t)
     ("u" toggle-frame-fullscreen "fullscreen" :exit t))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance"))
    "Split"
    (("r" split-window-right "horizontally")
     ("R" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Misc"
    (("o" set-frame-font "frame font")
     ("f" make-frame-command "new frame")
     ("d" delete-frame "delete frame")
     ("<left>" winner-undo "winner undo")
     ("<right>" winner-redo "winner redo"))))
  :bind (([remap other-window] . ace-window)
         ("C-c w" . ace-window-hydra/body))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))
      (user-error "`toggle-window-split' only supports two windows")))

  ;; Bind hydra to dispatch list
  (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Select the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))


;; EGLOT


(use-package eglot
  :ensure t
  :init
  (setq read-process-output-max (* 1024 1024) ;1 MB
        eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :ensure t
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :ensure t
  :after (consult eglot embark)
  :config
  (consult-eglot-embark-mode))

(use-package eldoc-box
  :ensure t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))


;; DEBUGGING


;; https://github.com/realgud/realgud
;; TODO - configure external debuggers from https://github.com/realgud/realgud/wiki/Debuggers-Available
(use-package realgud
  :ensure t)


;; FLYCHECK


;; https://github.com/flycheck/flycheck
;; TODO - configure
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


;; COMPILATION


;; https://github.com/mohkale/compile-multi
(use-package compile-multi
  :ensure t)

;; https://github.com/mohkale/compile-multi
(use-package consult-compile-multi
  :ensure t
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

;; https://github.com/mohkale/compile-multi
(use-package compile-multi-embark
  :ensure t
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))


;; RUN


;; https://github.com/emacsorphanage/quickrun
(use-package quickrun
  :ensure t)


;; FORMATTING


;; https://github.com/lassik/emacs-format-all-the-code
;; TODO - set formatters
(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))


;; GIT VERSION CONTROL


;; https://github.com/magit/magit
(use-package magit
  :ensure t)

;; https://github.com/Ailrun/magit-lfs
(use-package magit-lfs
  :ensure t
  :after magit)

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;; https://github.com/magit/forge
(use-package forge
  :ensure t
  :after magit)

;; https://github.com/magit/git-modes
(use-package git-modes
  :ensure t)

;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :ensure t
  :bind (( "C-c g g" . browse-at-remote))
  :config
  (setq browse-at-remote-add-line-number-if-no-region-selected nil))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

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


;; SNIPPETS


;;; https://github.com/minad/tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;; https://github.com/Crandel/tempel-collection
(use-package tempel-collection
  :ensure t
  :after tempel)


;; DIRED


;; TODO - copied from Centaur Emacs. Simplify!
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
           ("S" . hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package nerd-icons-dired
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
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
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package nerd-icons-dired
  :ensure t
  :after (nerd-icons dired)
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; IBUFFER

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))

;; https://github.com/muffinmad/emacs-ibuffer-project
(use-package ibuffer-project
  :ensure t
  :after (ibuffer)
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init
  (setq ibuffer-project-use-cache t))

;; TODO
;; Setup ibuffer hydra

(use-package nerd-icons-ibuffer
  :ensure t
  :after (nerd-icons ibuffer)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; PROCESS


;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 256 1024))  ; 256kb

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


;; DOCKER


;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


;; CHEATSHEET


;; https://github.com/mykyta-shyrin/cheatsheet
(use-package cheatsheet
  :ensure t)


;; DEVDOCS


;; https://github.com/astoff/devdocs.el
(use-package devdocs
  :ensure t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))


;; ELFEED


;; TODO - elfeed
;; TODO - elfeed-org


;; HTMLIZE


;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)


;; COMPILER EXPLORER


;; https://github.com/mkcms/compiler-explorer.el
(use-package compiler-explorer
  :ensure t)


;; PDF


;; TODO pdf-tools
;; TODO pdf-view-restore

(use-package pdf-tools
  :ensure t)


;; EPUB


(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


;; DISTRACTION FREE


(use-package writeroom-mode
  :ensure t
  :custom
  (writeroom-fullscreen-effect 'maximized))


;; GENERATE LINEAR RANGE


;; TODO - enable, interferes with C-; keybinding from iedit.
;; https://github.com/abo-abo/tiny
;;(use-package tiny
;;  :ensure t
;;  :config
;;  (tiny-setup-default))


;; ORG


;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :ensure t)

;; https://github.com/snosov1/toc-org
(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

;; https://github.com/rlister/org-present
(use-package org-present
  :ensure t)

;; https://github.com/minad/org-modern
(use-package org-modern
  :ensure t
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode))


;; SHELL


;; https://github.com/kyagi/shell-pop-el
(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-default-directory "~"
        shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell))))
        shell-pop-universal-key "C-t"
        shell-pop-window-size 30
        shell-pop-full-span t
        shell-pop-window-position "bottom"
        shell-pop-autocd-to-working-dir t
        shell-pop-restore-window-configuration t
        shell-pop-cleanup-buffer-at-process-exit t))

;; TODO - fzf - https://github.com/bling/fzf.el


;; ASSEMBLY


;; https://github.com/AdamNiederer/riscv-mode
(use-package riscv-mode
  :ensure t)


;; RUST


(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda () (prettify-symbols-mode)))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


;; SCHEME


;; https://github.com/emacsmirror/geiser
;;; TODO - check https://github.com/nbfalcon/macrostep-geiser
(use-package geiser
  :ensure t
  :after (consult-eglot)
  :init
  (consult-eglot-embark-mode))

;; https://gitlab.com/emacs-geiser/racket
(use-package geiser-racket
  :ensure t
  :after (geiser))

;; https://gitlab.com/emacs-geiser/chez
(use-package geiser-chez
  :ensure t
  :after (geiser))


;; MARKDOWN


;; https://jblevins.org/projects/markdown-mode/
;; TODO - check if we need to have https://github.com/nlamirault/emacs-markdownfmt
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; R


;; TODO - study settings
;; https://ess.r-project.org/Manual/ess.html
(use-package ess
  :ensure t)


;; POWERSHELL


;; https://github.com/jschaf/powershell.el
(use-package powershell
  :ensure t)


;; PYTHON


;; Install: pip install pyflakes autopep8
(use-package python
  :ensure exec-path-from-shell
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))


;; CSV MODE


;; https://elpa.gnu.org/packages/csv-mode.html
;; TODO - add pretty-hydra
(use-package csv-mode
  :ensure t
  :config
  (add-hook 'csv-mode-hook 'csv-guess-set-separator))
