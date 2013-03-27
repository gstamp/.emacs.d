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

            ;; conflict with kill opposite buffer
            (define-key ruby-mode-map "\C-c\C-l" nil)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace) 
            (company-mode)
            ))


(provide 'setup-ruby)
