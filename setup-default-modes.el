;; Load up some colour themes
(load-theme 'solarized-dark t)

;; c-c left, c-c right - to move between previous open window settings
(winner-mode 1)

(global-auto-revert-mode)  ; auto revert if there are external changes
(setq global-auto-revert-non-file-buffers t) ; auto revert dired as well
(setq auto-revert-verbose nil)
(global-linum-mode 0) ; line numbers on by default
(show-paren-mode t)   ; show matching parens


;; Set up dot mode, c-. to retype last thing you entered.
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(global-undo-tree-mode)

;; Recentf mode keeps track of recently opened files
(require 'recentf)
(recentf-mode 1)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Markdown doesn't always exist at the default location
(if (file-exists-p "/usr/local/bin/markdown")
    (setq markdown-command "/usr/local/bin/markdown"))

(provide 'setup-default-modes)
