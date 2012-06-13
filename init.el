;; TODO:
;; Document keys:
;;  - general emacs keys
;;  - redefined keys
;;  - bookmark mode
;;  - clojure mode
;;  - slime
;;  - feature-mode
;;  - expand-region
;;  - wgrep
;;  - mark-multiple
;;  - undo-tree
;; Install:
;;  - dot mode
;;  - js2-refactor.el
;;  - multiple-cursors.el
;;  - auto-complete
;; Fix broken plugins:
;;  - Durendal
;; 
;;  

;; Start by requiring and initializing the package manager.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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
                      clojure-test-mode
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
                      mark-multiple
                      undo-tree
                      projectile
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

;; Load up some colour themes
(load-theme 'solarized-dark t)

(require 'setup-defaults)
(require 'setup-grep)
(require 'setup-default-modes)
(require 'custom-defuns)

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f2] 'lgrep)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f6] 'save-and-compile)  ; Hit this to eval an entire file
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'ansi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f2)] 'multi-occur-in-this-mode)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(control f4)] 'kill-this-buffer)
(global-set-key "\C-c\C-l" 'kill-opposite-buffer)

(global-set-key [(shift f2)] 'bm-toggle)
(global-set-key [(shift f3)] 'bm-next)
(global-set-key [(shift f4)] 'bm-prev)
(global-set-key [(shift f6)] 'elein-swank)
(global-set-key [(control f6)] 'elein-reswank)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(require 'clojure-mode)
(define-key clojure-mode-map (kbd "C-c f") 'define-function)
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                                 (message "Dot mode activated.")))
(global-set-key "\C-c\C-v" 'slime-eval-print-last-expression)
(global-set-key "\C-x\C-m" 'execute-extended-command) ;; M-x replacement
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; M-x replacement
(define-key global-map (kbd "C-`") 'toggle-windows-split)
(global-set-key [(control tab)] 'other-window)
(global-set-key "\r" 'newline-and-indent)
(global-set-key [(control shift j)] 'join-with-next-line)
(global-set-key [(control c) (control a)] 'align-cljlet)
(global-set-key [(super up)] 'scroll-down)
(global-set-key [(super down)] 'scroll-up)
(global-set-key (kbd "<S-return>") 'open-line)
(global-set-key (kbd "C-S-o") '"\C-p\C-o") ; open line above
(global-set-key [home] 'smart-beginning-of-line)
;; You can use this mode to mark similar occuring text then type over it.
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-c C-,") 'slime-send-dwim)
(global-set-key (kbd "C-c C-.") '(lambda ()
                                   (interactive)
                                   (slime-send-dwim 1)))
;;(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; interactive search & replace c-; again to finish
(global-set-key [(control ";")] 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(eval-after-load 'paredit
  '(progn
     ;; Some paredit keybindings conflict with windmove and SLIME
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") nil)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map "\M-r" nil)))

;; Stops the mini buffer when switching back to emacs with mouse
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer. I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'qrr 'query-replace-regexp)

;;(add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)
;;(add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
;;(add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
;;(add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)

;; Indent tests correctly
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun)))

;; Redirect output from other threads.
;; Disabled - enabling this seems to cause bugs in slime
;; (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)


