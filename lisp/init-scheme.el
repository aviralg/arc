;; init-scheme.el --- Initialize Scheme -*- lexical-binding: t -*-

;; https://github.com/emacsmirror/geiser
;;; TODO - check https://github.com/nbfalcon/macrostep-geiser
(use-package geiser
  :ensure t)

;; https://gitlab.com/emacs-geiser/chez
(use-package geiser-chez
  :ensure t
  :after (geiser)
  :config
  (setq geiser-active-implementations '(chez))
  (setq geiser-chez-binary "chez"))

;; https://github.com/abo-abo/lispy
(use-package lispy
  :ensure
  :hook
  ((emacs-lisp-mode-hook . lispy-mode)
   (scheme-mode-hook . lispy-mode))
  :pretty-hydra
  ((:title "Lispy Actions" :foreign-keys warn :quit-key ("q" "C-g"))
   ("Ace"
    (("Q" special-lispy-ace-char "ace char")
     ("q" special-lispy-ace-paren "ace paren")
     ("-" special-lispy-ace-subword "ace subword")
     ("a" special-lispy-ace-symbol "ace jump")
     ("H" special-lispy-ace-symbol-replace "ace replace")
     ("v" special-lispy-view "view")
     ("V" special-lispy-visit "visit")
     ("A" special-lispy-beginning-of-defun "beginning of defun")
     ("P" special-lispy-paste "paste")
     ("R" special-lispy-raise-some "raise some")
     ("." special-lispy-repeat "repeat")
     ("u" special-lispy-undo "undo"))

    "Actions"
    (("y" special-lispy-occur "occur")
     ("r" special-lispy-raise "raise")
     ("<" special-lispy-barf "barf")
     (">" special-lispy-slurp "slurp")
     ("c" special-lispy-clone "clone")
     ("C" special-lispy-convolute "convolute")
     ("X" special-lispy-convolute-left "convolute left")
     ("M" special-lispy-alt-multiline "multiline")
     ("O" special-lispy-oneline "oneline")
     ("/" special-lispy-splice "splice"))

    "Move"
    (("h" special-lispy-left "←")
     ("j" special-lispy-down "↓")
     ("k" special-lispy-up "↑")
     ("l" special-lispy-right "→")
     ("?" special-lispy-move-left "move ←")
     ("s" special-lispy-move-down "move ↓")
     ("w" special-lispy-move-up "move ↑")
     ("?" special-lispy-move-right "move →"))

    "Eval"
    (("Z" special-lispy-edebug-stop "edebug stop")
     ("e" special-lispy-eval "eval")
     ("E" special-lispy-eval-and-insert "eval and insert")
     ("p" special-lispy-eval-other-window "eval other window"))

    "Miscellaneous 1"
    (("W" special-lispy-widen "widen")
     ("N" special-lispy-narrow "narrow")
     ("I" special-lispy-shifttab "shifttab")

     ("S" special-lispy-stringify "stringify")
     ("i" special-lispy-tab "tab")
     ("t" special-lispy-teleport "teleport")
     ("~" special-lispy-tilde "tilde")
     ("_" special-lispy-underscore "underscore")
     ("x" special-lispy-x "x")
     ("b" special-lispy-back "back")
     ("d" special-lispy-different "different")
     ("B" special-lispy-ediff-regions "ediff regions"))

    "Miscellaneous 2"
    (("f" special-lispy-flow "flow")
     ("F" special-lispy-follow "follow")
     ("G" special-lispy-goto "goto")
     ("g" special-lispy-goto-local "goto local")
     ("+" special-lispy-join "join")
     ("m" special-lispy-mark-list "mark list")
     ("n" special-lispy-new-copy "new copy")
     ("o" special-lispy-other-mode "other mode")
     ("L" special-lispy-outline-goto-child "goto child")
     ("J" special-lispy-outline-next "outline next")
     ("K" special-lispy-outline-prev "outline prev")))))

;;  https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
(use-package paren
  :ensure nil
  :config
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-in-periphery t))

(provide 'init-scheme)
