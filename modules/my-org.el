;;; modules/my-org.el --- Org mode -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :demand t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;;; --- Display ---
  ;; Clean visual appearance: indented headings, hidden markup markers,
  ;; syntax-highlighted code blocks, folded to heading level on open.
  (setq org-return-follows-link t
        org-startup-indented t
        org-startup-folded 'content
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-log-done 'time
        org-log-into-drawer t)

  ;;; --- Files ---
  ;; All org files under ~/org/. Agenda scans this directory.
  ;; Create ~/org/ before first use: mkdir -p ~/org
  (setq org-directory "~/org"
        org-agenda-files '("~/org")
        org-default-notes-file "~/org/inbox.org")

  ;;; --- TODO Workflow ---
  ;; TODO → IN-PROGRESS → WAITING → DONE / CANCELLED
  ;; @ suffix prompts for a note (why waiting? why cancelled?)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)")))

  ;;; --- Refile ---
  ;; Move headings to any file in ~/org/, up to 3 levels deep.
  ;; Shows full path in vertico for disambiguation. Auto-saves after.
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (advice-add 'org-refile :after #'org-save-all-org-buffers)

  ;;; --- Capture Templates ---
  ;; C-c c t — new TODO in inbox with timestamp and link back
  ;; C-c c n — quick note
  ;; C-c c j — journal entry filed under today's date
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/inbox.org")
           "* TODO %?\n%U\n%a" :empty-lines 1)
          ("n" "Note" entry (file "~/org/notes.org")
           "* %?\n%U" :empty-lines 1)
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n%U" :empty-lines 1)))

  ;;; --- Agenda ---
  ;; C-c a d — dashboard showing tasks grouped by state
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((todo "IN-PROGRESS" ((org-agenda-overriding-header "In Progress")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO" ((org-agenda-overriding-header "To Do"))))))))

(provide 'my-org)
;;; modules/my-org.el ends here
