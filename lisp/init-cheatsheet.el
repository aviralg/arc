

;; CHEATSHEET

;; https://github.com/mykyta-shyrin/cheatsheet
;; (use-package cheatsheet
;;  :ensure t)

(load-file "~/.emacs.d/cheatsheet.el")

(cheatsheet-add-group
 "Leaving Emacs"
 '(:key "C-z" :command suspend-frame :description "Iconify Emacs or suspend it in terminal")
 '(:key "C-x C-c" :command save-buffers-kill-terminal :description "Exit Emacs"))

(cheatsheet-add-group
 "Files"
 '(:key "C-x C-f" :command find-file :description "Read a file into Emacs" )
 '(:key "C-x C-s" :command save-buffer :description "Save a file back to disk")
 '(:key "C-x s"   :command save-some-buffers :description "Save all files")
 '(:key "C-x i"   :command insert-file :description "Insert contents of another file into this buffer")
 '(:key "C-x C-v" :command find-alternate-file :description "Replace this file with the file you really want")
 '(:key "C-x C-w" :command write-file :description "Write buffer to a specified file")
 '(:key "C-x C-q" :command read-only-mode :description "Toggle read-only status of buffer"))

(cheatsheet-add-group
 "Help"
 '(:key "C-h t" :command help-with-tutorial :description "Start tutorial")
 '(:key "C-x 1" :command delete-other-windows :description "remove help window")
 '(:key "C-M-v" :command scroll-other-window :description "scroll help window")
 '(:key "C-h a" :command apropos-command :description "apropos: show commands matching a string")
 '(:key "C-h k" :command describe-key :description "describe the function a key runs")
 '(:key "C-h f" :command describe-function :description "describe a function")
 '(:key "C-h m" :command describe-mode :description "get mode-specific information"))


(cheatsheet-add-group
 "Incremental Search"
 '(:key "C-s"   :command isearch-forward :description "search forward; repeat for next match")
 '(:key "C-r"   :command isearch-backward :description "search backward; repeat for previous match")
 '(:key "C-M-s" :command isearch-forward-regexp :description "regular expression search")
 '(:key "C-M-r" :command isearch-backward-regexp :description "reverse regular expression search")
 '(:key "M-p"   :command symbol-overlay-switch-backward :description "select previous search string")
 '(:key "M-n"   :command symbol-overlay-switch-forward :description "select next search string")
 '(:key "RET"   :command "" :description "exit incremental search")
 '(:key "DEL"   :command "" :description "undo effect of last character")
 '(:key "C-g"   :command "" :description "abort current search"))

;; (cheatsheet-add-group
;;  "Error Recovery"
;;  '(:key "C-g" :description "Abort partially typed or executing command")
;;  '(:key "M-x recover-session" :description "Recover files lost by a system crash")
;;  '(:key "C-x u, C-_ {\rm or} C-/" :description "Undo an unwanted change")
;;  '(:key "M-x revert-buffer" :description "Restore a buffer to its original contents")
;;  '(:key "C-l" :description "Redraw garbaged screen"))
;;
;; (cheatsheet-add-group
;;  'Motion
;;  '(:key "C-b" :description "Prev character")
;;  '(:key "C-f" :description "Next character")
;;  '(:key "M-b" :description "Prev word")
;;  '(:key "M-f" :description "Next word")
;;  '(:key "C-p" :description "Prev line")
;;  '(:key "C-n" :description "Next Line")
;;  '(:key "C-a" :description "Beginning of line")
;;  '(:key "C-e" :description "End of line")
;;  '(:key "M-a" :description "Beginning of sentence")
;;  '(:key "M-e" :description "End of sentence")
;;  '(:key "M-{" :description "Beginning of paragraph")
;;  '(:key "M-}" :description "End of paragraph")
;;  '(:key "C-x [" :description "Beg Page")
;;  '(:key "C-x ]" :description "End Page")
;;  '(:key "C-M-b" :description "Beg Sexp")
;;  '(:key "C-M-f" :description "End Sexp")
;;  '(:key "C-M-a" :description "Beg function")
;;  '(:key "C-M-e" :description "End function")
;;  '(:key "M-<" :description "Beginning of Buffer")
;;  '(:key "M->" :description "End of Buffer")
;;
;;  '(:key "C-v" :description "Scroll to next screen")
;;  '(:key "M-v" :description "Scroll to previous screen")
;;  '(:key "C-x <" :description "Scroll left")
;;  '(:key "C-x >" :description "Scroll right")
;;  '(:key "C-l" :description "Scroll current line to center, top, bottom")
;;  '(:key "M-g g" :description "Goto line")
;;  '(:key "M-g c" :description "Goto char")
;;  '(:key "M-m" :description "Back to indentation"))
;;
;; (cheatsheet-add-group
;;  'Kill
;;  '(:key "C-k"        :description "Kill from cursor to end of line")
;;  '(:key "M-d"        :description "Kill a word")
;;  '(:key "M-BS"       :description "Kill a word backward")
;;  '(:key "M-k"        :description "Kill from cursor to end of sentence")
;;  '(:key "C-w"        :description "Kill region")
;;  '(:key "M-z <char>" :description "Kill through next occurrence of <char>")
;;  '(:key "C-x BS"     :description "Kill backward to beginning of sentence"))
;;
;; (cheatsheet-add-group
;;  'Marking
;;  '(:key "C-@ • C-SPC" :description "Set mark here")
;;  '(:key "C-x C-x" :description "Exchange point and mark")
;;  '(:key "M-@" :description "Set mark <arg> words away")
;;  '(:key "M-h" :description "Mark paragraph")
;;  '(:key "C-x C-p" :description "Mark page")
;;  '(:key "C-M-@" :description "Mark sexp")
;;  '(:key "C-M-h" :description "Mark function")
;;  '(:key "C-x h" :description "Mark buffer"))
;;
;; (cheatsheet-add-group
;;  'Help
;;  '(:key "C-h t" :description "Open tutorial")
;;  '(:key "C-x 1" :description "remove help window")
;;  '(:key "C-M-v" :description "scroll help window")
;;  '(:key "C-h a" :description "apropos: show commands matching a string")
;;  '(:key "C-h k" :description "describe the function a key runs")
;;  '(:key "C-h f" :description "describe a function")
;;  '(:key "C-h m" :description "get mode-specific information"))
;;
;; (cheatsheet-add-group
;;  'Rectangles
;;  '(:key "C-x r r" :description "Copy rectangle to register")
;;  '(:key "C-x r k" :description "Kill rectangle")
;;  '(:key "C-x r y" :description "Yank rectangle")
;;  '(:key "C-x r o" :description "Open rectangle, shifting text right")
;;  '(:key "C-x r c" :description "Blank out rectangle")
;;  '(:key "C-x r t" :description "Prefix each line of rectangle with a string"))
;;
;; (cheatsheet-add-group
;;  'Shells
;;  '(:key "M-!" :description "Execute a shell command synchronously")
;;  '(:key "M-&" :description "Execute a shell command asynchronously")
;;  '(:key "M-|" :description "Run a shell command on the region")
;;  '(:key "C-u M-|" :description "Filter region through a shell command")
;;  '(:key "M-x shell" :description "Start a shell in window *shell*"))
;;
;; (cheatsheet-add-group
;;  'Buffers
;;  '(:key "C-x b" :description "Select another buffer")
;;  '(:key "C-x C-b" :description "List all buffers")
;;  '(:key "C-x k" :description "Kill a buffer"))
;;
;; (cheatsheet-add-group
;;  'Transposing
;;  '(:key "C-t" :description "Transpose characters")
;;  '(:key "M-t" :description "Transpose words")
;;  '(:key "C-x C-t" :description "Transpose lines")
;;  '(:key "C-M-t" :description "Transpose sexps"))
;;
;; (cheatsheet-add-group
;;  'Registers
;;  '(:key "C-x r s" :description "Save region in register")
;;  '(:key "C-x r i" :description "Insert register contents into buffer")
;;  '(:key "C-x r SPC" :description "Save value of point in register")
;;  '(:key "C-x r j" :description "Jump to point saved in register"))
;;
;; (cheatsheet-add-group
;;  'Elisp
;;  '(:key "C-x C-e" :description "Eval sexp before point")
;;  '(:key "C-M-x" :description "Eval current defun")
;;  '(:key "M-x eval-region" :description "Eval region")
;;  '(:key "M-:" :description "Read and eval minibuffer")
;;  '(:key "M-x load-library" :description "Load a Lisp library from load-path"))

(provide 'init-cheatsheet)
