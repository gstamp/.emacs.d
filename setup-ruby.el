(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

;; Local key bindings
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key [(control c) (control e)] 'ruby-insert-end)
            (local-set-key [(control meta f1)] 'xmp) ;; gem install rcodetools
            (local-set-key [(control meta shift f1)] 'ruby-eval-buffer)

            (company-mode)
            ))

(add-hook 'before-save-hook (lambda ()
                              (if (string= major-mode "ruby-mode")
                                  (whitespace-cleanup))))


(provide 'setup-ruby)
