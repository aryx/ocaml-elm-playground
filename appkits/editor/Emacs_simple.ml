(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Emacs_simple.mli *)

let text =
  {|
;;; the variables the command loop sets

(defvar last-command nil "The command run before this one.")
(defvar this-command nil "The command running; a command may change it, for the next.")
(defvar last-command-event 0 "The last key typed, as a character.")
(defvar current-prefix-arg nil "The prefix argument given by C-u, or nil.")
(defvar mode-name "Fundamental" "The current buffer's major mode, by name.")

;;; the keymap

(defvar global-map nil
  "The key bindings: an alist of key sequences and commands, the newest first.")

(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND."
  (setq global-map (cons (cons key command) global-map)))

(defun key-binding (key)
  "Return the command KEY runs, or nil."
  (cdr (assoc key global-map)))

;;; typing

(defun self-insert-command (n)
  "Insert the character you type, N times."
  (interactive "p")
  (insert (make-string n last-command-event)))

(defun newline (n)
  "Insert a newline, N times."
  (interactive "p")
  (insert (make-string n ?\n)))

(defun open-line (n)
  "Insert a newline after point, point staying before it."
  (interactive "p")
  (insert (make-string n ?\n))
  (backward-char n))

(defun delete-backward-char (n)
  "Delete the N characters before point."
  (interactive "p")
  (delete-char (- n)))

(defun tab-to-tab-stop ()
  "Insert spaces up to the next tab stop, every 8 columns."
  (interactive)
  (insert (make-string (- 8 (% (current-column) 8)) ?\s)))

(defun transpose-chars ()
  "Interchange the characters around point, moving forward one; at the
end of a line, the two before it."
  (interactive)
  (if (eolp) (backward-char 1))
  (let ((a (char-before)) (b (char-after)))
    (if (null a) (signal 'beginning-of-buffer nil))
    (delete-char -1)
    (delete-char 1)
    (insert b a)))

(defun delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive)
  (while (memq (char-before) '(32 9)) (delete-char -1))
  (while (memq (char-after) '(32 9)) (delete-char 1)))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive)
  (delete-horizontal-space)
  (insert " "))

;;; moving

(defvar temporary-goal-column 0
  "The column C-n and C-p aim for, kept while they follow each other.")

(defun line-move (n)
  (unless (memq last-command '(next-line previous-line))
    (setq temporary-goal-column (current-column)))
  (if (/= (forward-line n) 0)
      (signal (if (< n 0) 'beginning-of-buffer 'end-of-buffer) nil)
    (move-to-column temporary-goal-column)))

(defun next-line (n)
  "Move cursor vertically down N lines, to the column it started from."
  (interactive "p")
  (line-move n))

(defun previous-line (n)
  "Move cursor vertically up N lines."
  (interactive "p")
  (line-move (- n)))

(defun word-char-p (c)
  (and c (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)) (and (>= c ?0) (<= c ?9)))))

(defun forward-word (n)
  "Move point forward N words."
  (interactive "p")
  (dotimes (i (or n 1))
    (while (and (not (eobp)) (not (word-char-p (char-after)))) (forward-char 1))
    (while (word-char-p (char-after)) (forward-char 1))))

(defun backward-word (n)
  "Move point backward N words."
  (interactive "p")
  (dotimes (i (or n 1))
    (while (and (not (bobp)) (not (word-char-p (char-before)))) (backward-char 1))
    (while (word-char-p (char-before)) (backward-char 1))))

(defun beginning-of-buffer ()
  "Move point to the beginning of the buffer, leaving the mark where it was."
  (interactive)
  (set-mark (point))
  (goto-char (point-min)))

(defun end-of-buffer ()
  "Move point to the end of the buffer, leaving the mark where it was."
  (interactive)
  (set-mark (point))
  (goto-char (point-max)))

(defun goto-line (n)
  "Go to line N, counting from 1."
  (interactive "nGoto line: ")
  (goto-char (point-min))
  (forward-line (1- n)))

(defun count-lines (start end)
  "Return the number of newlines between START and END."
  (save-excursion
    (goto-char start)
    (let ((n 0))
      (while (search-forward "\n" end t) (setq n (1+ n)))
      n)))

(defun what-line ()
  "Print the current line number."
  (interactive)
  (message "Line %d" (1+ (count-lines (point-min) (line-beginning-position)))))

;;; the mark and the region

(defun set-mark-command ()
  "Set the mark where point is."
  (interactive)
  (set-mark (point))
  (message "Mark set"))

(defun exchange-point-and-mark ()
  "Put the mark where point is, and point where the mark was."
  (interactive)
  (let ((m (mark)))
    (unless m (error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char m)))

(defun mark-whole-buffer ()
  "Put point at the beginning of the buffer and the mark at its end."
  (interactive)
  (set-mark (point-max))
  (goto-char (point-min)))

(defun count-words-region (start end)
  "Say how many lines, words and characters the region has."
  (interactive "r")
  (let ((words 0) (done nil))
    (save-excursion
      (goto-char start)
      (while (not done)
        (forward-word 1)
        (if (and (<= (point) end) (word-char-p (char-before))) (setq words (1+ words)))
        (if (or (eobp) (>= (point) end)) (setq done t))))
    (message "Region has %d lines, %d words, and %d characters."
             (count-lines start end) words (- end start))))

;;; killing and yanking

(defun kill-region (start end)
  "Kill the text between START and END: deleted, and saved in the kill
ring. Kills that follow each other make one entry."
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (if (eq last-command 'kill-region)
        (kill-append text (< end start))
      (kill-new text))
    (delete-region start end)
    (setq this-command 'kill-region)))

(defun kill-ring-save (start end)
  "Save the region in the kill ring, as if killed, without deleting it."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (message "Saved the region"))

(defun kill-line ()
  "Kill the rest of the current line; if only its newline is left, kill that."
  (interactive)
  (kill-region (point)
               (if (eolp)
                   (if (eobp) (signal 'end-of-buffer nil) (1+ (point)))
                 (line-end-position))))

(defun kill-word (n)
  "Kill characters forward until the end of a word, N times."
  (interactive "p")
  (kill-region (point) (progn (forward-word n) (point))))

(defun backward-kill-word (n)
  "Kill characters backward until the start of a word, N times."
  (interactive "p")
  (kill-region (point) (progn (backward-word n) (point))))

(defun yank ()
  "Insert the latest kill, the mark left at its start."
  (interactive)
  (set-mark (point))
  (insert (current-kill 0))
  (setq this-command 'yank))

(defun yank-pop ()
  "Replace the text just yanked with the kill before it."
  (interactive)
  (unless (eq last-command 'yank) (error "Previous command was not a yank"))
  (delete-region (mark) (point))
  (set-mark (point))
  (insert (current-kill 1))
  (setq this-command 'yank))

(defun upcase-word (n)
  "Convert to upper case the N words after point."
  (interactive "p")
  (let ((start (point)))
    (forward-word n)
    (let ((s (upcase (buffer-substring start (point)))))
      (delete-region start (point))
      (insert s))))

(defun downcase-word (n)
  "Convert to lower case the N words after point."
  (interactive "p")
  (let ((start (point)))
    (forward-word n)
    (let ((s (downcase (buffer-substring start (point)))))
      (delete-region start (point))
      (insert s))))

;;; Lisp

(defun preceding-sexp ()
  (let ((end (point)))
    (car (read-from-string (buffer-substring (save-excursion (backward-sexp) (point)) end)))))

(defun eval-last-sexp ()
  "Evaluate the expression before point; show its value in the echo area."
  (interactive)
  (message "%S" (eval (preceding-sexp))))

(defun eval-print-last-sexp ()
  "Evaluate the expression before point, and insert its value after it."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (insert "\n" (prin1-to-string value) "\n")))

(defun eval-expression (form)
  "Evaluate FORM, read in the minibuffer, and show its value."
  (interactive "xEval: ")
  (message "%S" (eval form)))

(defun eval-buffer ()
  "Evaluate every expression of the buffer, in order."
  (interactive)
  (let ((text (buffer-string)) (pos 0) (n 0))
    (condition-case nil
        (while t
          (let ((r (read-from-string (substring text pos))))
            (eval (car r))
            (setq pos (+ pos (cdr r)))
            (setq n (1+ n))))
      (end-of-file nil))
    (message "Evaluated %d expressions" n)))

(defun newline-or-eval ()
  "C-j: in Lisp Interaction mode, the expression before point evaluated
and its value printed; a newline anywhere else."
  (interactive)
  (if (equal mode-name "Lisp Interaction") (eval-print-last-sexp) (newline 1)))

;;; buffers and files

(defun list-buffers ()
  "Show the buffers, in the buffer *Buffer List*."
  (interactive)
  (let ((names (buffer-list)))
    (switch-to-buffer "*Buffer List*")
    (erase-buffer)
    (insert " Buffers:\n\n")
    (dolist (name names)
      (unless (equal name "*Buffer List*") (insert "  " name "\n")))
    (insert "\n Files:\n\n")
    (dolist (file (directory-files)) (insert "  " file "\n"))
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun save-buffers-kill-emacs ()
  "Save the buffers visiting files, and exit."
  (interactive)
  (dolist (name (buffer-list))
    (switch-to-buffer name)
    (if (and (buffer-file-name) (buffer-modified-p)) (save-buffer)))
  (kill-emacs))

(defun help-with-tutorial ()
  "Visit the tutorial."
  (interactive)
  (find-file "TUTORIAL"))

;;; help: Emacs documents itself

(defun describe-function (f)
  "Show the documentation of the function F."
  (interactive "aDescribe function: ")
  (message "%s%s: %s" f (if (commandp f) " (a command)" "")
           (or (documentation f) "Not documented.")))

(defun describe-key (key)
  "Show the command KEY runs, and its documentation."
  (interactive "kDescribe key: ")
  (let ((command (key-binding key)))
    (if command
        (message "%s runs %s: %s" (key-description key) command
                 (or (documentation command) "Not documented."))
      (message "%s is undefined" (key-description key)))))

(defun keyboard-quit ()
  "Cancel what was being typed: a prefix, a minibuffer."
  (interactive)
  (message "Quit"))

(defun execute-extended-command (command)
  "Read a command's name in the minibuffer, and run it."
  (interactive "CM-x ")
  (call-interactively command))

;;; the bindings (bindings.el's)

(global-set-key "\r" 'newline)
(global-set-key "\t" 'tab-to-tab-stop)
(global-set-key "\C-?" 'delete-backward-char)
(global-set-key "\C-d" 'delete-char)
(global-set-key "\e[3~" 'delete-char)
(global-set-key "\C-f" 'forward-char)
(global-set-key "\C-b" 'backward-char)
(global-set-key "\C-n" 'next-line)
(global-set-key "\C-p" 'previous-line)
(global-set-key "\e[C" 'forward-char)
(global-set-key "\e[D" 'backward-char)
(global-set-key "\e[B" 'next-line)
(global-set-key "\e[A" 'previous-line)
(global-set-key "\C-a" 'beginning-of-line)
(global-set-key "\C-e" 'end-of-line)
(global-set-key "\e[H" 'beginning-of-line)
(global-set-key "\e[F" 'end-of-line)
(global-set-key "\M-f" 'forward-word)
(global-set-key "\M-b" 'backward-word)
(global-set-key "\M-<" 'beginning-of-buffer)
(global-set-key "\M->" 'end-of-buffer)
(global-set-key "\C-v" 'scroll-up-command)
(global-set-key "\M-v" 'scroll-down-command)
(global-set-key "\e[6~" 'scroll-up-command)
(global-set-key "\e[5~" 'scroll-down-command)
(global-set-key "\C-l" 'recenter)
(global-set-key "\M-gg" 'goto-line)
(global-set-key "\C-o" 'open-line)
(global-set-key "\C-t" 'transpose-chars)
(global-set-key "\M-\\" 'delete-horizontal-space)
(global-set-key "\M- " 'just-one-space)
(global-set-key "\M-u" 'upcase-word)
(global-set-key "\M-l" 'downcase-word)
(global-set-key "\M-=" 'count-words-region)
(global-set-key "\C-@" 'set-mark-command)
(global-set-key "\C-x\C-x" 'exchange-point-and-mark)
(global-set-key "\C-xh" 'mark-whole-buffer)
(global-set-key "\C-k" 'kill-line)
(global-set-key "\C-w" 'kill-region)
(global-set-key "\M-w" 'kill-ring-save)
(global-set-key "\M-d" 'kill-word)
(global-set-key "\M-\C-?" 'backward-kill-word)
(global-set-key "\C-y" 'yank)
(global-set-key "\M-y" 'yank-pop)
(global-set-key "\C-_" 'undo)
(global-set-key "\C-xu" 'undo)
(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-j" 'newline-or-eval)
(global-set-key "\C-x\C-e" 'eval-last-sexp)
(global-set-key "\M-:" 'eval-expression)
(global-set-key "\M-x" 'execute-extended-command)
(global-set-key "\C-x\C-f" 'find-file)
(global-set-key "\C-x\C-s" 'save-buffer)
(global-set-key "\C-x\C-w" 'write-file)
(global-set-key "\C-xb" 'switch-to-buffer)
(global-set-key "\C-x\C-b" 'list-buffers)
(global-set-key "\C-xk" 'kill-buffer)
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)
(global-set-key "\C-g" 'keyboard-quit)
(global-set-key "\C-u" 'universal-argument)
(global-set-key "\C-hk" 'describe-key)
(global-set-key "\C-hf" 'describe-function)
(global-set-key "\C-ht" 'help-with-tutorial)
|}
