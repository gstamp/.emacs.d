

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (if (not (file-exists-p "images"))
      (make-directory "images"))
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (setq just-filename (car (last (split-string filename "/") )))
  (call-process "import" nil nil nil filename)
  (rename-file filename "images/")
  (insert (concat "[[./images/" just-filename "]]"))
  (org-display-inline-images))

(provide 'setup-orgmode)

