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

;; Save the buffer before running the tests
(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun live-transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)

(defun clojure-hide-successful-compile (msg)
  ;;When you add a hook to the slime compilation-finished hook it
  ;;seems to override the default behaviour. We I manually call the
  ;;oringal code.
  (slime-maybe-show-compilation-log msg)
  (with-struct (slime-compilation-result. notes duration successp)
      slime-last-compilation-result
    (when successp
      (if (get-buffer "*SLIME Compilation*")
          (progn
            (kill-buffer "*SLIME Compilation*")
            (when (> (length (window-list)) 1)
              (delete-window (next-window))
              ))))))

(add-hook 'slime-compilation-finished-hook 'clojure-hide-successful-compile)


;; Redirect output from other threads.
;; Disabled - enabling this seems to cause bugs in slime
;; (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)

(provide 'setup-clojure)
