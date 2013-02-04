(require 'git-gutter)

;; Update changes information after save buffer
(add-hook 'after-save-hook
          (lambda ()
            (when (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
              (git-gutter))))

(provide 'setup-git-gutter)
