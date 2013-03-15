;; TODO: not sure if this is working
(setq grep-find-command
      "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -I -n -e ")
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files ".tmp")
     (add-to-list 'grep-find-ignored-directories ".svn")))
(grep-compute-defaults)

(setq ack-and-a-half-root-directory-functions 'ack-and-a-half-guess-project-root)

(provide 'setup-grep)
