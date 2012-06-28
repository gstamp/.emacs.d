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

(provide 'setup-completion)
