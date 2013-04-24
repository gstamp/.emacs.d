(require 'git-gutter)

(defun git-gutter-save-hook ()
  (when (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
    (git-gutter)))

;; Update changes information after save buffer
(add-hook 'after-save-hook 'git-gutter-save-hook)

(provide 'setup-git-gutter)
