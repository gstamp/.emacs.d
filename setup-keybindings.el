(require 'clojure-mode)
(require 'align-cljlet)
(require 'mark-multiple)
(require 'mark-more-like-this)
(require 'paredit)

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f2] 'ack-and-a-half)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f5] 'ert-run)
(global-set-key [f6] 'save-and-compile)  ; Hit this to eval an entire file
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f9] 'org-agenda)
(global-set-key [f10] 'multi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f1)] 'toggle-selective-display)
(global-set-key [(control f2)] 'multi-occur-in-this-mode)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(control f4)] 'kill-this-buffer)
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key [(control f5)] 'linum-mode)
(global-set-key [(control f6)] 'elein-reswank)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(global-set-key [(shift f2)] 'bm-toggle)
(global-set-key [(shift f3)] 'bm-next)
(global-set-key [(shift f4)] 'bm-prev)
(global-set-key [(shift f6)] 'elein-swank)
(global-set-key [(shift f9)] 'toggle-window-split)
(global-set-key [(shift f10)] 'rotate-windows)

(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
(global-set-key "\C-c\C-l" 'kill-opposite-buffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x o") 'find-file-in-project)
;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))
;; Store an org mode link C-cC-l to use it.
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

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
(global-set-key (kbd "<S-return>") 'open-line)
(global-set-key (kbd "C-S-o") '"\C-p\C-o") ; open line above
(global-set-key [home] 'smart-beginning-of-line)
(define-key dot-mode-map (kbd "C->") nil) ; fix conflict with dot-mode
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c C-,") 'slime-send-dwim)
(global-set-key (kbd "C-c C-.") '(lambda ()
                                   (interactive)
                                   (slime-send-dwim 1)))

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; bind git-gutter toggle command
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

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


;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer. I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))


(provide 'setup-keybindings)
