;; Emac 24 configuration for Glen Stampoultzis
;;
;; See README.md for more details.  The latest version of this
;; configuration can be found at https://github.com/gstamp/.emacs.d
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup: Helper macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is simply a wrapper around eval-after-load.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup: Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start by requiring and initializing the package manager.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; This is the list of packages we want installed by default.
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-ruby
                      starter-kit-lisp
                      starter-kit-js
                      clojure-mode
                      align-cljlet
                      cljdoc
                      midje-mode
                      slime
                      solarized-theme
                      yaml-mode
                      bm
                      sgml-mode
                      expand-region
                      elein
                      rainbow-delimiters
                      feature-mode
                      groovy-mode
                      undo-tree
                      projectile
                      markdown-mode
                      graphviz-dot-mode
                      dired-details
                      auto-complete
                      ac-slime
                      highlight-symbol
                      wgrep
                      htmlize
                      yasnippet
                      csv-mode
                      slime-repl
                      rinari
                      rspec-mode
                      zencoding-mode
                      color-theme-sanityinc-tomorrow
                      ack-and-a-half
                      exec-path-from-shell
                      multi-term
                      coffee-mode
                      sass-mode
                      company
                      puppet-mode
                      tabkey2
                      robe
                      ruby-mode
                      git-gutter
                      company-inf-ruby
                      github-browse-file
                      key-chord
                      fiplr
                      multiple-cursors
                      golden-ratio
                      powerline
                      flx
                      flx-ido
                      )
  "A list of packages to ensure are installed at launch.")

;; Install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; Set up load path
(add-to-list 'load-path dotfiles-dir)

;; When starting GUI from mac correctly initialize PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup site-lisp autoloads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some sane defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load up some colour themes
(color-theme-sanityinc-tomorrow-eighties)

;; JS indent levels
(setq js-indent-level 2)
(setq js2-basic-offset 2)

;; By default, Emacs inserts tabs in place of multiple spaces when it
;; formats a region. We want spaces.
(setq-default intent-tabs-mode nil)

;; Setup flx (https://github.com/lewang/flx)
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Indenting defaults
(setq nxml-child-indent 4)
(setq lua-indent-level 4)

;; Allow emacs to access the x clipboard
(setq x-select-enable-clipboard t)

;; Set the GC limit to 40MB to avoid over GCage.
(setq gc-cons-threshold 40000000)

;;  The variable vc-follow-symlinks controls what Emacs does if you
;;  try to visit a symbolic link pointing to a version-controlled
;;  file.
(setq vc-follow-symlinks t)

;; flash instead of ding
(setq visible-bell t)

;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; Put backups in ~/.emacsbackup/ and control how many versions are kept
(setq backup-directory-alist
      (list (cons ".*" (expand-file-name "~/.emacsbackup/")))) 
(setq version-control t)                ; backup multiple versions
(setq delete-old-versions t)            ; delete the older ones
(setq kept-new-versions 10)             ; keep x new ones
(setq kept-old-versions 3)              ; keep x old ones

;; TODO: Not sure this is actually doing anything
;; Sets up whitepsace.
;;  trailing - no trailing whitespace and end of line
;;  lines - lines whose have columns beyond ‘whitespace-line-column’
;;          are highlighted via faces.
;;  tabs - TABs are visualized via faces.
(setq whitespace-style '(trailing lines tabs)
      whitespace-line-column 80)        ; no trailing space or tabs

;; gerkin config
(setq feature-default-language "en")

;; Display/redraw defaults
(setq set-cursor-type 'box)
(set-cursor-color "yellow")

;; Can't see a good reason not to enable these commands
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Emacs starter kit turns on flyspell by default but I prefer it off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Emacs starter kit also turns on idle-highlight-mode.  Not a fan.
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

;; Redraw more frequently
(setq redisplay-dont-pause 't)

;; Add some extra snippets
(require 'yasnippet)
(setq yas/snippet-dirs
      '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; Stop ERC telling me about all those people joining/quiting.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Increase find file in project file limit
(setq ffip-limit 3500)

(setq feature-cucumber-command "bundle exec cucumber {options} {feature}")

(when (equal system-type 'darwin)
  (progn
    ;; map meta to the command key on mac
    (setq mac-option-key-is-meta nil)
    (setq mac-left-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)

    ;; I like the right option key to be control because there's no
    ;; right control key on the mac
    (setq mac-right-option-modifier 'control)

    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)

    ;; keybinding to toggle full screen mode
    (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

    ;; Move to trash when deleting stuff
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs")

    (setenv "LANG" "en_AU.UTF-8")

    (defadvice ansi-term (after advise-ansi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'ansi-term)
    (defadvice multi-term (after advise-multi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'multi-term)
    
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    
    ;; mac friendly font
    (if window-system
        (set-face-attribute 'default nil :font "Monaco-12"))
    ))

;; Open the buffer list in the same window
(add-to-list 'same-window-buffer-names "*Buffer List*")

;; Add rainbow delimiters to all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; Alternatively it can be hooked up to specific modes:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; ..or globally..
;; (global-rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Keychord mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'key-chord)

(key-chord-define-global "BB" 'ido-switch-buffer)
(key-chord-define-global "FF" 'fiplr-find-file)
(key-chord-define-global "SS" 'save-buffer)
(key-chord-define-global "ww" 'window-configuration-to-register)
(key-chord-define-global "wj" 'jump-to-register)
(key-chord-define-global "jk" 'beginning-of-buffer)
(key-chord-define-global "jl" 'end-of-buffer)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" 'switch-to-previous-buffer)

(key-chord-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after 'paredit
       ;; making paredit work with delete-selection-mode
       (put 'paredit-forward-delete 'delete-selection 'supersede)
       (put 'paredit-backward-delete 'delete-selection 'supersede)
       (put 'paredit-open-round 'delete-selection t)
       (put 'paredit-open-square 'delete-selection t)
       (put 'paredit-doublequote 'delete-selection t)
       (put 'paredit-newline 'delete-selection t)

       (defun paredit-wrap-round-from-behind ()
         (interactive)
         (forward-sexp -1)
         (paredit-wrap-round)
         (insert " ")
         (forward-char -1))

       ;; Paredit improvements pinched from emacs-live
       (defun live-paredit-next-top-level-form ()
         (interactive)
         (while (ignore-errors (paredit-backward-up) t))
         (live-paredit-forward))

       (defun live-paredit-previous-top-level-form ()
         (interactive)
         (if (ignore-errors (paredit-backward-up) t)
             (while (ignore-errors (paredit-backward-up) t))
           (paredit-backward)))

       (defun live-paredit-forward ()
         "Feels more natural to move to the beginning of the next item
in the sexp, not the end of the current one."
         (interactive)
         (if (and (not (paredit-in-string-p))
                  (save-excursion
                    (ignore-errors
                      (forward-sexp)
                      (forward-sexp)
                      t)))
             (progn
               (forward-sexp)
               (forward-sexp)
               (backward-sexp))
           (paredit-forward)))

       (defun live-paredit-reindent-defun (&optional argument)
         "Reindent the definition that the point is on. If the point is
  in a string or a comment, fill the paragraph instead, and with
  a prefix argument, justify as well. Doesn't mess about with
  Clojure fn arglists when filling-paragraph in docstrings."
         (interactive "P")
         (cond ((paredit-in-comment-p) (fill-paragraph argument))
               ((paredit-in-string-p) (progn
                                        (save-excursion
                                          (paredit-forward-up)
                                          (insert "\n"))
                                        (fill-paragraph argument)
                                        (save-excursion
                                          (paredit-forward-up)
                                          (delete-char 1))))
               (t (save-excursion
                    (end-of-defun)
                    (beginning-of-defun)
                    (indent-sexp)))))

       (defun live-paredit-forward-kill-sexp (&optional arg)
         (interactive "p")
         (cond ((or (paredit-in-comment-p)
                    (paredit-in-string-p)) (kill-word (or arg 1)))
               (t (kill-sexp (or arg 1)))))

       (defun live-paredit-backward-kill-sexp (&optional arg)
         (interactive "p")
         (cond ((or (paredit-in-comment-p)
                    (paredit-in-string-p)) (backward-kill-word (or arg 1)))
               (t (backward-kill-sexp (or arg 1)))))

       (defun paredit--is-at-start-of-sexp ()
         (and (looking-at "(\\|\\[")
              (not (nth 3 (syntax-ppss))) ;; inside string
              (not (nth 4 (syntax-ppss))))) ;; inside comment

       (defun paredit-duplicate-closest-sexp ()
         (interactive)
         ;; skips to start of current sexp
         (while (not (paredit--is-at-start-of-sexp))
           (paredit-backward))
         (set-mark-command nil)
         ;; while we find sexps we move forward on the line
         (while (and (bounds-of-thing-at-point 'sexp)
                     (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                     (not (= (point) (line-end-position))))
           (forward-sexp)
           (while (looking-at " ")
             (forward-char)))
         (kill-ring-save (mark) (point))
         ;; go to the next line and copy the sexprs we encountered
         (paredit-newline)
         (yank)
         (exchange-point-and-mark))


       ;; Some paredit keybindings conflict with windmove and SLIME,
       ;; adjust those and make some new bindings.
       (define-key paredit-mode-map (kbd "<C-left>") nil)
       (define-key paredit-mode-map (kbd "<C-right>") nil)
       (define-key paredit-mode-map "\M-r" nil)
       (define-key paredit-mode-map (kbd "C-M-f") 'live-paredit-forward)
       (define-key paredit-mode-map (kbd "C-M-k") 'live-paredit-forward-kill-sexp)
       (define-key paredit-mode-map (kbd "C-M-<backspace>") 'live-paredit-backward-kill-sexp)
       (define-key paredit-mode-map (kbd "M-q") 'live-paredit-reindent-defun)
       (define-key paredit-mode-map (kbd "M-<up>") 'live-paredit-previous-top-level-form)
       (define-key paredit-mode-map (kbd "M-<down>") 'live-paredit-next-top-level-form)
       (define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
       (define-key paredit-mode-map (kbd "C-S-d") 'paredit-duplicate-closest-sexp)
       
       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Git Gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'git-gutter)

(global-git-gutter-mode t)

;; bind git-gutter toggle command
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: CSV Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup: Ack and a Half
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ack-and-a-half-root-directory-functions 'ack-and-a-half-guess-project-root)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Window Rearrangement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-key global-map (kbd "C-|") 'toggle-windows-split)

(defun toggle-split-direction ()
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
          (if this-win-2nd (other-window 1))))))

(global-set-key [(shift f9)] 'toggle-split-direction)

(defun split-window-right-and-choose-last-buffer ()
  "Like split-window-right but selects the last buffer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-next-buffer)
  (other-window -1))

(global-set-key (kbd "C-x 3") 'split-window-right-and-choose-last-buffer)

(defun split-window-below-and-choose-last-buffer ()
  "Like split-window-below but selects the last buffer"
  (interactive)
  (split-window-below)
  (other-window 1)
  (switch-to-next-buffer)
  (other-window -1))

(global-set-key (kbd "C-x 2") 'split-window-below-and-choose-last-buffer)

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key [(shift f10)] 'rotate-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Buffer Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Editor helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(defun join-with-next-line ()
  "join with next line"
  (interactive)
  (next-line)
  (delete-indentation))

(global-set-key [(control shift j)] 'join-with-next-line)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; TODO: setup a keybinding for this


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Make open-line work more like VI (bound to ctrl-o)
(defadvice open-line (before new-open-line activate)
  (end-of-visible-line))
(defadvice open-line (after after-open-line activate)
  (forward-line 1)
  (indent-according-to-mode))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) 
  (if (not (string-equal major-mode "org-mode"))
      (let ((oldpos (point)))
        (back-to-indentation)
        (and (= oldpos (point))
             (beginning-of-line)))
    (move-beginning-of-line nil)))

(global-set-key [home] 'smart-beginning-of-line)

;; this will indent the yanked region automatically in the provided
;; modes
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode
                                           lisp-mode
                                           clojure-mode
                                           ruby-mode
                                           c-mode
                                           c++-mode
                                           objc-mode
                                           LaTeX-mode
                                           TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))

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

(defun mark-line-or-next ()
  "Marks the current line or extends the mark if there is no current selection"
  (interactive)
  (if mark-active
      (forward-line)
    (progn
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
      (forward-char)
      (activate-mark))))

(global-set-key (kbd "C-;") 'mark-line-or-next)

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stops the mini buffer when switching back to emacs with mouse
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun ert-run ()
  "Evaluate the current buffer and run ert testing framework"
  (interactive)

  (eval-buffer)
  (ert 't)
  )
(global-set-key [f5] 'ert-run)

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'align-cljlet)

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f2] 'ack-and-a-half)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'multi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(control f4)] 'kill-this-buffer)
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key [(control f5)] 'linum-mode)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Selects the definition which encloses the point
(global-set-key (kbd "C-M-h") 'mark-defun)

(global-set-key "\C-x\C-m" 'execute-extended-command) ;; M-x replacement
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; M-x replacement
(global-set-key [(control tab)] 'other-window)
(global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "<S-return>") 'open-line)
(global-set-key (kbd "C-S-o") '"\C-p\C-o") ; open line above

(global-set-key (kbd "C-=") 'er/expand-region)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer. I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

;; Reload dired after quitting wdired
(defadvice wdired-abort-changes (after revert-buffer-after-abort activate)
  (revert-buffer))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;; M-up is nicer in dired if it moves to the third line - straight to the ".."
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top)

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1)
  (dired-back-to-start-of-files))

(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (kbd "M-<up>") 'dired-back-to-top)
     (define-key wdired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: HS Minor Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'Fix' 'WARNING: terminal is not fully functional' from less/etc.
(setenv "PAGER" "cat")

;; Tell Riniari about extra prompt patterns
(after 'rinari-minor-mode
  (setq rinari-inf-ruby-prompt-pattern
        (concat rinari-inf-ruby-prompt-pattern "\\|\\(.*»\\)")))

(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(require 'rspec-mode)
(require 'bundler)

(after 'rspec-mode

  (defun rspec-run-and-arrange ()
    (interactive)

    (if (not (rspec-buffer-is-spec-p))
        (rspec-toggle-spec-and-target))

    (rspec-verify)
    (delete-other-windows)
    (if (get-buffer "*rspec-compilation*")
        (let ((result-window (split-window-below)))

          (set-window-buffer result-window "*rspec-compilation*")
          (rspec-toggle-spec-and-target)

          (split-window-right-and-choose-last-buffer)

          (enlarge-window 10)
          )))

  (define-key rspec-mode-map (kbd "M-\"") 'rspec-run-and-arrange)
  )

;; Local key bindings

(defun init-rubymode ()
  (local-set-key [(control c) (control e)] 'ruby-insert-end)
  (local-set-key [(control meta f1)] 'xmp) ;; gem install rcodetools
  (local-set-key [(control meta shift f1)] 'ruby-eval-buffer)

  (company-mode)
  nil)

(after 'ruby-mode
  (init-rubymode))

(after 'company
  (add-to-list 'company-backends 'company-inf-ruby))

(add-hook 'before-save-hook (lambda ()
                              (if (string= major-mode "ruby-mode")
                                  (whitespace-cleanup))))

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

(defun turn-on-hs-minor-mode ()
  (hs-minor-mode 1))

(add-hook 'ruby-mode-hook 'turn-on-hs-minor-mode)

;; Init robe
(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'ac-slime)
(require 'slime)

(defun start-autocomplete ()
  (auto-complete-mode)
  (set-up-slime-ac))

;;(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'start-autocomplete)
(add-hook 'slime-repl-mode-hook 'start-autocomplete)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't use the standard org keys for todo and priority management
;; http://orgmode.org/manual/Conflicts.html
(setq org-replace-disputed-keys 1)
(setq org-confirm-babel-evaluate nil)

;; Colour org mode source code
(setq org-src-fontify-natively 1)
;; Valid task states in org mode
;; Shift left/right switches between modes in the current sequence.
;; Control shift left/right switches to a different sequence.
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE")
        (sequence "ONHOLD" "|" "CANCELLED")))
;; When a task is finished log when it's done
(setq org-log-done 'time)

(after 'org

  (require 'org-html5presentation)

  (defun org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (if (not (file-exists-p "images"))
        (make-directory "images"))
    (setq filename
          (concat
           (make-temp-name
            (concat (buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (setq just-filename (car (last (split-string filename "/") )))
    (call-process "import" nil nil nil filename)
    (rename-file filename "images/")
    (insert (concat "[[./images/" just-filename "]]"))
    (org-display-inline-images))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (sh . t)
     (js . t)
     (java . t)
     (awk . t)
     (sql . t)
     (ruby . t)
     ))

  ;; Store an org mode link C-cC-l to use it.
  (define-key org-mode-map "\C-cl" 'org-store-link)
  )

(global-set-key [f9] 'org-agenda)

(setq org-export-html-style-extra "<style type=\"text/css\">\n  html {\n  font-family: sans-serif;\n  font-size: 11pt;\n  }\n  em { font-style: normal; font-weight: bold;}\n</style>")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make ido open file list vertically
(setq ido-max-prospects 60)
(setq ido-max-file-prompt-width 0.8)
(setq ido-max-window-height 30)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(setq ffip-full-paths 't) ; Not really part of ido but works well with
                                        ; vertically displayed lists.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; full screen magit-status

;; This code makes magit-status run alone in the frame, and then
;; restores the old window configuration when you quit out of magit.

(require 'magit)


(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; This adds W to toggle ignoring whitespace in magit.

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; C-d on an empty line in the shell terminates the process.

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
                          (if (> (length git-output) 0)
                              (substring git-output 0 -1)
                            "(no branch)")
                          "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
                        (if (> (length p-lst) 3)
                            (concat
                             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                   (substring elm 0 1)))
                                        (butlast p-lst 3)
                                        "/")
                             "/"
                             (mapconcat (lambda (elm) elm)
                                        (last p-lst 3)
                                        "/"))
                          (mapconcat (lambda (elm) elm)
                                     p-lst
                                     "/")))
                      (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))

(setq eshell-highlight-prompt nil)

(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-window-height 30)
(global-set-key (kbd "M-g M-l") 'shell-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'zencoding-mode)

(add-hook 'sgml-mode-hook 'zencoding-mode) ; Auto-start on any markup modes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You'll need to:
;;  gem install terminal-notifier
;; for this to work and be on OS/X 10.8
(defun osx-notify (title message)
  "Shows a MESSAGE with TITLE using OSX Mountain Lion notifications"
  (shell-command-to-string
   (format "terminal-notifier -title '%s' -message '%s'"
           title message)))

(defun osx-notify-erc-hook (match-type nick message)
  "Notify when nick is mentioned"
  (when (eq match-type 'current-nick)
    (osx-notify (buffer-name (current-buffer)) message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'restclient)
(require 'rcodetools)
(require 'rwd-bell)
(require 'tabkey2) ; Make tabkey2 on by default
(tabkey2-mode)

;; c-c left, c-c right - to move between previous open window settings
(winner-mode 1)

;; Delete selection when typing over it
(delete-selection-mode)

(global-auto-revert-mode)  ; auto revert if there are external changes
(setq global-auto-revert-non-file-buffers t) ; auto revert dired as well
(setq auto-revert-verbose nil)
(global-linum-mode 0) ; line numbers on by default
(show-paren-mode t)   ; show matching parens

(global-undo-tree-mode)

;; Recentf mode keeps track of recently opened files
(require 'recentf)
(recentf-mode 1)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Markdown doesn't always exist at the default location
(if (file-exists-p "/usr/local/bin/markdown")
    (setq markdown-command "/usr/local/bin/markdown"))

;; Puppet mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; yaml setup
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Revert ESK default. Don't autofill comments.
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Save Place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customize save places not to save for certain file types.
(require 'saveplace)
(setq save-place-skip-check-regexp
      (concat
       save-place-skip-check-regexp
       "\\|\\.org$"
       "\\|\\.\\(arc\\|lzh\\|zip\\|zoo\\)$"
       "\\|\\.t\\(ar\\.\\)?gz$"
       "\\|\\.t\\(ar\\.bz2\\|bz\\)$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Slime And Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode)

(after `clojure-mode-autoloads

  ;; ignore slime complaining about the version mismatch (& set default
  ;; ports)
  (setq slime-protocol-version 'ignore)
  (defvar slime-port 4005)
  (defvar durendal-port 4005)

  ;; defaults to iso-8895-1  encoding otherwise.
  (setq slime-net-coding-system 'utf-8-unix)

  ;; stop slime giving me annoying messages
  (setq font-lock-verbose nil)

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
  (define-key clojure-mode-map (kbd "C-c -") 'slime-send-dwim)
  (define-key clojure-mode-map (kbd "C-c =") '(lambda ()
                                                (interactive)
                                                (slime-send-dwim 1)))

  ;; TODO: This wants to be a real function
  (fset 'save-and-compile
        "\C-x\C-s\C-c\C-k")

  (global-set-key [f6] 'save-and-compile)  ; Hit this to eval an entire file

  )

;; Indent tests correctly
(after 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun))
  

  ;; Save the buffer before running the tests
  (defadvice clojure-test-run-tests (before save-first activate)
    (save-buffer))

  ;;Treat hyphens as a word character when transposing words
  (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
    (let ((st (make-syntax-table clojure-mode-syntax-table)))
      (modify-syntax-entry ?- "w" st)
      st))
  ;; Pinched from Programothesis.  Thanks!
  (defun define-function ()
    (interactive)
    (let ((name (symbol-at-point)))
      (backward-paragraph)
      (insert "\n(defn " (symbol-name name) " [])\n")
      (backward-char 3)))

  (define-key clojure-mode-map (kbd "C-c f") 'define-function)
  (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
  (define-key clojure-mode-map [(shift f6)] 'elein-swank)
  (define-key clojure-mode-map [(control f6)] 'elein-reswank)
  (define-key clojure-mode-map "\C-c\C-v" 'slime-eval-print-last-expression)

  (defun live-transpose-words-with-hyphens (arg)
    "Treat hyphens as a word character when transposing words"
    (interactive "*p")
    (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
      (transpose-words arg)))

  (define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)

  (defun clojure-hide-successful-compile (msg)
    ;;When you add a hook to the slime compilation-finished hook it
    ;;seems to override the default behaviour. So I manually call the
    ;;original code.
    (slime-maybe-show-compilation-log msg)
    
    (with-struct (slime-compilation-result. notes duration successp)
        slime-last-compilation-result
      (when successp
        (dolist (w (window-list))
          (if (string= (buffer-name (window-buffer w)) "*SLIME Compilation*")
              (progn
                (kill-buffer "*SLIME Compilation*")
                (delete-window w)
                ))))))

  (add-hook 'slime-compilation-finished-hook 'clojure-hide-successful-compile)


  ;; Redirect output from other threads.
  ;; Disabled - enabling this seems to cause bugs in slime
  ;; (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)

  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable undo in region
(setq undo-in-region t)

(after 'undo-tree-autoloads
  (setq undo-tree-enable-undo-in-region t)

  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Multiple Cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after 'multiple-cursors

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
  (global-set-key (kbd "C-*") 'mc/mark-all-dwim)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(require 'multiple-cursors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Bookmark Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after 'bm-autoloads
  ;; Part of bookmarks plugin.  Only highlight bookmarks on fringe.
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  (global-set-key [(shift f2)] 'bm-toggle)
  (global-set-key [(shift f3)] 'bm-next)
  (global-set-key [(shift f4)] 'bm-prev)
  (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Fiplr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For finding files in a project

(after 'fiplr

  (setq *grizzl-read-max-results* 18)
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "vendor" "tmp"))
                              (files ("*.jpg" "*.png" "*.zip" "*~"))))
  (global-set-key (kbd "C-x o") 'fiplr-find-file)
  (global-set-key (kbd "C-x C-o") 'fiplr-find-file))

(require 'fiplr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Golden ratio plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'golden-ratio)

(golden-ratio-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: IMenu Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When invoking imenu this creates a 'Sections' entry that you can
;; use to jump between sections in this file.  A section is any
;; comment starting with 4 semicolons.
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)




