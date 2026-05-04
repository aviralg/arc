;;; modules/cheatsheet.el --- Keybinding cheatsheet browser -*- lexical-binding: t -*-
;;
;; Provides:
;;   (my/cheatsheet-add SECTION COMMAND DESCRIPTION)
;;   M-x my/cheatsheet  — open the *Cheatsheet* buffer (C-c ?)
;;
;; Entries are registered in cheatsheet-entries.el.
;; Click a command name  → describe-function
;; Click a keybinding    → describe-key
;; Click a package name  → describe-package
;; TAB on section header → toggle collapse
;; /   → filter entries (minibuffer)
;; C-g → clear active filter

;;; Data model -------------------------------------------------------------

(defvar my/cheatsheet--entries nil
  "Alist of (SECTION ENTRY...) where each ENTRY is (COMMAND DESCRIPTION).
Populated by `my/cheatsheet-add'.  Never manipulated directly.")

;; Collapsed sections: a hash-set of section name strings.
(defvar-local my/cheatsheet--collapsed (make-hash-table :test 'equal))

;; Current filter string.
(defvar-local my/cheatsheet--filter "")

(defun my/cheatsheet--detect-key (command)
  "Return the shortest keybinding string for COMMAND, or nil."
  (when-let ((keys (where-is-internal command nil nil :no-remap)))
    (key-description
     (car (seq-sort-by #'length #'< keys)))))

(defun my/cheatsheet--detect-package (command)
  "Return the package name where COMMAND is defined, or \"built-in\"."
  (if-let ((file (symbol-file command 'defun)))
      (file-name-base file)
    "built-in"))

(defun my/cheatsheet-add (section command description)
  "Register a cheatsheet entry.
SECTION     — string grouping header, e.g. \"Navigation\"
COMMAND     — symbol, e.g. \\='iedit-mode
DESCRIPTION — short one-line string"
  (let ((entry (list command description))
        (cell  (assoc section my/cheatsheet--entries)))
    (if cell
        (setcdr cell (append (cdr cell) (list entry)))
      (push (list section entry) my/cheatsheet--entries))))

;;; Faces ------------------------------------------------------------------

(defface my/cheatsheet-section-header
  '((t :inherit (bold)
       :height 1.1))
  "Face for cheatsheet section headings.")

(defface my/cheatsheet-command-name
  '((t :inherit link :underline nil))
  "Face for clickable command names in the cheatsheet.")

(defface my/cheatsheet-keybinding
  '((((class color) (background light))
     :foreground "#005f5f"
     :weight bold)
    (((class color) (background dark))
     :foreground "#5fd7af"
     :weight bold))
  "Face for keybinding strings in the cheatsheet.")

(defface my/cheatsheet-description
  '((t :inherit default))
  "Face for description text in the cheatsheet.")

(defface my/cheatsheet-package
  '((((class color) (background light))
     :foreground "#5f7f5f"
     :slant italic)
    (((class color) (background dark))
     :foreground "#87af87"
     :slant italic))
  "Face for package name in the cheatsheet.")

(defface my/cheatsheet-column-header
  '((t :inherit (bold shadow)))
  "Face for the column header row.")

(defface my/cheatsheet-match
  '((t :inherit match))
  "Face for matched text in filtered results.")

;;; Column layout ----------------------------------------------------------

(defconst my/cheatsheet--col-command 28
  "Width of the command-name column in characters.")

(defconst my/cheatsheet--col-key 16
  "Width of the keybinding column in characters.")

(defconst my/cheatsheet--col-package 20
  "Width of the package column in characters.")

(defun my/cheatsheet--pad (str width)
  "Return STR left-aligned in a field of WIDTH characters, truncating if needed."
  (let ((len (length str)))
    (if (>= len width)
        (concat (substring str 0 (- width 1)) " ")
      (concat str (make-string (- width len) ?\s)))))

;;; Filter -----------------------------------------------------------------

(defun my/cheatsheet--entry-matches-p (command description filter)
  "Return non-nil if COMMAND/DESCRIPTION match FILTER string."
  (let ((name    (symbol-name command))
        (key     (or (my/cheatsheet--detect-key command) ""))
        (package (my/cheatsheet--detect-package command))
        (pattern (regexp-quote filter)))
    (or (string-match-p pattern name)
        (string-match-p pattern key)
        (string-match-p pattern package)
        (string-match-p pattern description))))

(defun my/cheatsheet--filtered-entries ()
  "Return entries alist filtered by `my/cheatsheet--filter'."
  (if (string-empty-p my/cheatsheet--filter)
      my/cheatsheet--entries
    (let (result)
      (dolist (section-data my/cheatsheet--entries)
        (let ((section (car section-data))
              (matches (cl-remove-if-not
                        (lambda (entry)
                          (my/cheatsheet--entry-matches-p
                           (car entry) (cadr entry) my/cheatsheet--filter))
                        (cdr section-data))))
          (when matches
            (push (cons section matches) result))))
      (nreverse result))))

;;; Click handlers ---------------------------------------------------------

(defun my/cheatsheet--command-map (command)
  "Return a keymap that calls `describe-function' for COMMAND."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda (_e) (interactive "e") (describe-function command)))
    (define-key map (kbd "RET")
                (lambda () (interactive) (describe-function command)))
    map))

(defun my/cheatsheet--key-map (key)
  "Return a keymap that calls `describe-key' for KEY."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda (_e) (interactive "e")
                  (describe-key (kbd key))))
    (define-key map (kbd "RET")
                (lambda () (interactive)
                  (describe-key (kbd key))))
    map))

(defun my/cheatsheet--package-map (package)
  "Return a keymap that calls `describe-package' for PACKAGE."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda (_e) (interactive "e")
                  (describe-package (intern package))))
    (define-key map (kbd "RET")
                (lambda () (interactive)
                  (describe-package (intern package))))
    map))

(defun my/cheatsheet--section-toggle-map (section)
  "Return a keymap that toggles collapse for SECTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda (_e) (interactive "e")
                  (my/cheatsheet--toggle-section section)))
    (define-key map (kbd "TAB")
                (lambda () (interactive)
                  (my/cheatsheet--toggle-section section)))
    map))

;;; Collapse ---------------------------------------------------------------

(defun my/cheatsheet--collapsed-p (section)
  "Return non-nil if SECTION is collapsed."
  (gethash section my/cheatsheet--collapsed))

(defun my/cheatsheet--toggle-section (section)
  "Toggle collapsed state for SECTION and re-render."
  (if (my/cheatsheet--collapsed-p section)
      (remhash section my/cheatsheet--collapsed)
    (puthash section t my/cheatsheet--collapsed))
  (my/cheatsheet--render))

;;; Buffer rendering -------------------------------------------------------

(defun my/cheatsheet--insert-filter-status ()
  "Insert a filter-status line when a filter is active; nothing otherwise."
  (unless (string-empty-p my/cheatsheet--filter)
    (insert (propertize "  Filter: " 'face '(bold shadow)))
    (insert (propertize my/cheatsheet--filter 'face 'my/cheatsheet-keybinding))
    (insert (propertize "  (C-g to clear)" 'face 'shadow))
    (insert "\n\n")))

(defun my/cheatsheet--insert-header ()
  "Insert the title banner, filter line, and column headers."
  (let ((total (apply #'+ (mapcar (lambda (s) (length (cdr s)))
                                  my/cheatsheet--entries)))
        (sections (length my/cheatsheet--entries)))
    (insert (propertize "Cheatsheet\n" 'face '(bold (:height 1.3))))
    (insert (propertize (format "%d entries  ·  %d sections\n\n"
                                total sections)
                        'face 'shadow)))
  (my/cheatsheet--insert-filter-status)
  (insert (propertize
           (concat "      "
                   (my/cheatsheet--pad "Command" my/cheatsheet--col-command)
                   "  "
                   (my/cheatsheet--pad "Key" my/cheatsheet--col-key)
                   "  "
                   (my/cheatsheet--pad "Package" my/cheatsheet--col-package)
                   "  Description\n")
           'face 'my/cheatsheet-column-header)))

(defun my/cheatsheet--insert-section-header (section section-num)
  "Insert a styled section header for SECTION with SECTION-NUM.
The heading text and separator together span exactly 120 characters."
  (let* ((collapsed (my/cheatsheet--collapsed-p section))
         (triangle  (if collapsed "▶ " "▼ "))
         (heading   (format "%s%d. %s" triangle section-num section))
         (gap       4)
         (sep-len   (max 0 (- 120 (length heading) gap)))
         (spaces    (make-string gap ?\s))
         (rule      (make-string sep-len ?─)))
    (insert "\n")
    (insert (propertize (concat heading spaces rule)
                        'face       'my/cheatsheet-section-header
                        'mouse-face 'highlight
                        'help-echo  "mouse-1 / TAB: toggle section"
                        'keymap     (my/cheatsheet--section-toggle-map section)
                        'my/cheatsheet-section section))
    (insert "\n")))

(defun my/cheatsheet--insert-row (command description row-num)
  "Insert one table row for COMMAND / DESCRIPTION with ROW-NUM."
  (let* ((name    (symbol-name command))
         (key     (or (my/cheatsheet--detect-key command) ""))
         (package (my/cheatsheet--detect-package command))
         (cmd-str (my/cheatsheet--pad name    my/cheatsheet--col-command))
         (key-str (my/cheatsheet--pad key     my/cheatsheet--col-key))
         (pkg-str (my/cheatsheet--pad package my/cheatsheet--col-package)))
    (insert (propertize (format "  %2d. " row-num) 'face 'shadow))
    (insert (propertize cmd-str
                        'face       'my/cheatsheet-command-name
                        'mouse-face 'highlight
                        'help-echo  (format "mouse-1: describe %s" name)
                        'keymap     (my/cheatsheet--command-map command)))
    (insert "  ")
    (if (string-empty-p key)
        (insert (make-string my/cheatsheet--col-key ?\s))
      (insert (propertize key-str
                          'face       'my/cheatsheet-keybinding
                          'mouse-face 'highlight
                          'help-echo  (format "mouse-1: describe key %s" key)
                          'keymap     (my/cheatsheet--key-map key))))
    (insert "  ")
    (if (string-empty-p package)
        (insert (make-string my/cheatsheet--col-package ?\s))
      (insert (propertize pkg-str
                          'face       'my/cheatsheet-package
                          'mouse-face 'highlight
                          'help-echo  (format "mouse-1: describe package %s" package)
                          'keymap     (my/cheatsheet--package-map package))))
    (insert "  ")
    (insert (propertize description 'face 'my/cheatsheet-description))
    (insert "\n")))

(defun my/cheatsheet--render ()
  "Erase and fully repopulate the *Cheatsheet* buffer."
  (let ((inhibit-read-only t)
        (saved-pos (point)))
    (erase-buffer)
    (my/cheatsheet--insert-header)
    (let ((section-num 0))
      (dolist (section-data
               (sort (copy-sequence (my/cheatsheet--filtered-entries))
                     (lambda (a b) (string< (car a) (car b)))))
        (cl-incf section-num)
        (let ((section (car section-data)))
          (my/cheatsheet--insert-section-header section section-num)
          (unless (my/cheatsheet--collapsed-p section)
            (let ((row-num 0))
              (dolist (entry (cdr section-data))
                (cl-incf row-num)
                (apply #'my/cheatsheet--insert-row (append entry (list row-num)))))))))
    (insert "\n")
    (goto-char (min saved-pos (point-max)))))

;;; Filter commands --------------------------------------------------------

(defun my/cheatsheet-filter (filter)
  "Prompt for a FILTER string and re-render the cheatsheet."
  (interactive
   (list (read-string
          (if (string-empty-p my/cheatsheet--filter)
              "Filter: "
            (format "Filter (current: %s): " my/cheatsheet--filter))
          nil nil my/cheatsheet--filter)))
  (setq my/cheatsheet--filter filter)
  (my/cheatsheet--render))

(defun my/cheatsheet-filter-clear ()
  "Clear the active filter and re-render."
  (interactive)
  (setq my/cheatsheet--filter "")
  (my/cheatsheet--render))

;;; Major mode -------------------------------------------------------------

(defvar-keymap my/cheatsheet-mode-map
  :doc "Keymap for `my/cheatsheet-mode'."
  "g"         #'my/cheatsheet-refresh
  "q"         #'quit-window
  "n"         #'next-line
  "p"         #'previous-line
  "/"         #'my/cheatsheet-filter
  "C-g"       #'my/cheatsheet-filter-clear
  "<tab>"     #'my/cheatsheet-tab
  "<backtab>" #'backward-button)

(defun my/cheatsheet-tab ()
  "TAB: toggle section if on a header, otherwise move to next button."
  (interactive)
  (if-let ((section (get-text-property (point) 'my/cheatsheet-section)))
      (my/cheatsheet--toggle-section section)
    (forward-button 1)))

(define-derived-mode my/cheatsheet-mode special-mode "Cheatsheet"
  "Major mode for browsing the keybinding cheatsheet.
\\{my/cheatsheet-mode-map}"
  (setq-local truncate-lines   nil
              word-wrap         t
              line-spacing      0
              buffer-read-only  t)
  (hl-line-mode -1))

;;; Entry points -----------------------------------------------------------

(defun my/cheatsheet-refresh ()
  "Refresh the *Cheatsheet* buffer in place."
  (interactive)
  (when-let ((buf (get-buffer "*Cheatsheet*")))
    (with-current-buffer buf
      (my/cheatsheet--render)
      (message "Cheatsheet refreshed."))))

;;;###autoload
(defun my/cheatsheet ()
  "Open the *Cheatsheet* browser."
  (interactive)
  (let ((buf (get-buffer-create "*Cheatsheet*")))
    (with-current-buffer buf
      (unless (eq major-mode 'my/cheatsheet-mode)
        (my/cheatsheet-mode))
      (my/cheatsheet--render))
    (switch-to-buffer buf)))

(keymap-global-set "C-c ?" #'my/cheatsheet)

;;; modules/cheatsheet.el ends here
