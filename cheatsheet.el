;;; cheatsheet.el --- create your own cheatsheet
;;; Commentary:

;; Quick start:
;; Load package
;; Add your first cheat:
;; (cheatsheet-add :group 'Common
;;                 :key "C-x C-c"
;;                 :description "leave Emacs.")
;; Run (cheatsheet-show) and enjoy looking at your own Emacs cheatsheet.

;;; Code:

(require 'cl-lib)

;; :foreground ,(face-background font-lock-constant-face)
;; :background ,(face-foreground font-lock-constant-face))

(defface cheatsheet-group-face
  '((t :inherit font-lock-constant-face
       :box t
       :height 1.1))
  "Group name font face.")

(defface cheatsheet-key-face
  '((t :inherit font-lock-keyword-face))
  "Cheat key font face.")

(defface cheatsheet-command-face
  '((t :inherit font-lock-function-name-face))
  "Cheat command font face.")

(defface cheatsheet-description-face
  '((t :inherit font-lock-string-face))
  "Cheat description font face.")

(defvar cheatsheet--cheat-list '()
  "List of cheats.")

;; Getters for CHEAT and GROUP plists
(defun cheatsheet--if-symbol-to-string (string-like)
  "Convert STRING-LIKE to string."
  (if (symbolp string-like) (symbol-name string-like) string-like))

(defun cheatsheet--group-name (group)
  "Get GROUP name."
  (cheatsheet--if-symbol-to-string (plist-get group :name)))

(defun cheatsheet--group-cheats (group)
  "Get GROUP cheats."
  (cheatsheet--if-symbol-to-string (plist-get group :cheats)))

(defun cheatsheet--cheat-key (cheat)
  "Get CHEAT key."
  (cheatsheet--if-symbol-to-string (plist-get cheat :key)))

(defun cheatsheet--cheat-command (cheat)
  "Get CHEAT command."
  (cheatsheet--if-symbol-to-string (plist-get cheat :command)))

(defun cheatsheet--cheat-group (cheat)
  "Get CHEAT group."
  (cheatsheet--if-symbol-to-string (plist-get cheat :group)))

(defun cheatsheet--cheat-description (cheat)
  "Get CHEAT description."
  (cheatsheet--if-symbol-to-string (plist-get cheat :description)))

;; Functions to get data from CHEATSHEET in convenient format
(defun cheatsheet--cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups
            (mapcar 'cheatsheet--cheat-group
                    cheatsheet--cheat-list))))

(defun cheatsheet--get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (cl-flet ((is-current-group (cheat)
                              (if (string= (cheatsheet--cheat-group cheat)
                                           group)
                                  cheat
                                nil)))
    (delq nil (mapcar #'is-current-group cheatsheet--cheat-list))))

(defun cheatsheet--describe-key (button)
  (describe-key (kbd (button-label button))))

(defun cheatsheet--describe-command (button)
  (describe-function (intern (button-label button))))

(defun cheatsheet--key-button (faced-key key)
  (insert-button faced-key
                 'button t
                 'face 'cheatsheet-key-face
                 ;;'category t
                 'font-lock-face 'button
                 'action 'cheatsheet--describe-key))

(defun cheatsheet--command-button (faced-command command)
  (insert-button faced-command
                 'button t
                 'face 'cheatsheet-command-face
                 ;;'category t
                 'font-lock-face 'button
                 'action 'cheatsheet--describe-command))

;; Functions to format cheatsheet items and prepare to print
(defun cheatsheet--format-cheat (cheat key-cell-length command-cell-length)
  "Format CHEAT row with KEY-CELL-LENGTH key cell length and COMMAND-CELL-LENGTH command cell length."
  (let* ((format-string (format "%%%ds - %%%ds - %%s\n" key-cell-length command-cell-length))
         (key (cheatsheet--cheat-key cheat))
         (command (cheatsheet--cheat-command cheat))
         (description (cheatsheet--cheat-description cheat))
         (faced-key (propertize key 'face 'cheatsheet-key-face))
         (faced-command (propertize command 'face 'cheatsheet-command-face))
         (faced-description (propertize description 'face 'cheatsheet-description-face)))
    (insert " • ")
    (cheatsheet--key-button faced-key key)
    (insert (make-string (- key-cell-length (length key)) ?\s))
    (insert "- ")
    (cheatsheet--command-button faced-command command)
    (insert (make-string (- command-cell-length (length command)) ?\s))
    (insert " - ")
    (insert faced-description)
    (insert "\n")))
    ;;(format format-string faced-key faced-command faced-description)))

(defun cheatsheet--format-group (group)
  "Format GROUP to table."
  (cl-flet ((key-length (cheat) (length (cheatsheet--cheat-key cheat)))
            (command-length (cheat) (length (cheatsheet--cheat-command cheat)))
            (format-cheat (key-cell-length command-cell-length cheat)
                          (cheatsheet--format-cheat cheat key-cell-length command-cell-length)))
    (let* ((name (format " %s " (cheatsheet--group-name group)))
           (cheats (cheatsheet--group-cheats group))
           (key-max-length (apply 'max (mapcar #'key-length cheats)))
           (key-cell-length (+ 2 key-max-length))
           (command-max-length (apply 'max (mapcar #'command-length cheats)))
           (command-cell-length command-max-length)
           (format-cheat (apply-partially #'format-cheat key-cell-length command-cell-length))
           ;;(formatted-cheats (apply 'concat (mapcar format-cheat cheats)))
           (faced-group-name (propertize name 'face 'cheatsheet-group-face)))
      (insert faced-group-name)
      (insert "\n")
      (mapc format-cheat cheats)
      (insert "\n"))))
      ;;(concat faced-group-name "\n" formatted-cheats "\n"))))

(defun cheatsheet--format ()
  "Print the whole cheatsheet."
  (let* ((cheatsheet (cheatsheet-get)))
         ;;(formatted-groups (mapcar 'cheatsheet--format-group cheatsheet))
         ;;(formatted-cheatsheet (apply 'concat formatted-groups)))
    (mapc #'cheatsheet--format-group cheatsheet)))
    ;;formatted-cheatsheet))

;; Interface
;;;###autoload
(defun cheatsheet-add (&rest cheat)
  "Add CHEAT to cheatsheet."
  (add-to-list 'cheatsheet--cheat-list cheat))

(defun cheatsheet-get ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (cl-flet ((make-group (group)
                        (list :name group
                              :cheats (cheatsheet--get-group group))))
    (mapcar #'make-group (cheatsheet--cheat-groups))))

;;;###autoload
(defun cheatsheet-add-group (group &rest cheats)
  "Add cheats to the same group."
  (mapcar #'(lambda (cheat)
              (apply 'cheatsheet-add
                     (append `(:group ,group) cheat)))
          cheats))

;;;###autoload
(defun cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (switch-to-buffer-other-window "*cheatsheet*")
  (cheatsheet-mode)
  (erase-buffer)
  (cheatsheet--format)
  ;;(insert (cheatsheet--format))
  (setq buffer-read-only t))

(define-derived-mode cheatsheet-mode fundamental-mode "Cheat Sheet"
  "Set major mode for viewing cheat sheets.")

(define-key cheatsheet-mode-map (kbd "C-q") 'kill-buffer-and-window)

(provide 'cheatsheet)
;;; cheatsheet.el ends here
