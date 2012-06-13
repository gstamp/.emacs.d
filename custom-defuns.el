(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u)))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun join-with-next-line ()
  "join with next line"
  (interactive)
  (next-line)
  (delete-indentation) ; Join this line to previous and
                                        ; fix up whitepace at line
  )

(defun slime-send-dwim (arg)
  "Send the appropriate forms to REPL to be evaluated."
  (interactive "P")
  (save-excursion
    (cond 
     ;;Region selected - evaluate region
     ((not (equal mark-active nil))
      (copy-region-as-kill (mark) (point)))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "(")
          (save-excursion
            (ignore-errors (forward-char 1))
            (looking-at "(")))
      (forward-list 1)
      (let ((end (point))
            (beg (save-excursion
                   (backward-list 1)
                   (point))))
        (copy-region-as-kill beg end)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at ")")
          (save-excursion
            (backward-char 1)
            (looking-at ")")))
      (if (looking-at ")")
          (forward-char 1))
      (let ((end (point))
            (beg (save-excursion
                   (backward-list 1)
                   (point))))
        (copy-region-as-kill beg end)))
     ;; Default - evaluate enclosing top-level sexp
     (t (progn
          (while (ignore-errors (progn
                                  (backward-up-list)
                                  t)))
          (forward-list 1)
          (let ((end (point))
                (beg (save-excursion
                       (backward-list 1)
                       (point))))
            (copy-region-as-kill beg end)))))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (yank)
    (if arg (progn
	      (slime-repl-return)
	      (other-window 1)))))

;; duplicate line - requires open line from below.
(global-set-key "\C-cd" "\C-a\C-k\C-y\C-o\C-y")

;; Make open-line work more like VI (bound to ctrl-o)
(defadvice open-line (before new-open-line activate)
  (end-of-visible-line))
(defadvice open-line (after after-open-line activate)
  (forward-line 1)
  (indent-according-to-mode))

(setq slime-protocol-version 'ignore)

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; this will indent the yanked region automatically in the provided
;; modes
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           clojure-mode
                                           c-mode c++-mode objc-mode
                                           LaTeX-mode TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Pinched from Programothesis.  Thanks!
(defun define-function ()
  (interactive)
  (let ((name (symbol-at-point)))
    (backward-paragraph)
    (insert "\n(defn " (symbol-name name) " [])\n")
    (backward-char 3)))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(fset 'save-and-compile
      "\C-x\C-s\C-c\C-k")

(fset 'kill-opposite-buffer
      "\C-xo\C-xk\C-m\C-xo")

(provide 'custom-defuns)
