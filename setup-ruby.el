(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

;; Local key bindings
(add-hook 'ruby-mode-hook
          (lambda ()
            ;; (ruby-electric-mode)
            (local-set-key [(control c) (control e)] 'ruby-insert-end)
            (local-set-key [(control meta f1)] 'xmp) ;; gem install rcodetools
            (local-set-key [(control meta shift f1)] 'ruby-eval-buffer)
            (local-set-key (kbd "TAB") 'smart-tab)
            ))

(provide 'setup-ruby)
