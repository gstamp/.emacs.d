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

(provide 'setup-clojure)
