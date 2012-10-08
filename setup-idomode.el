
;; Show ido completions vertically (seems broken going up a dir)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (if ido-temp-list
            (sort ido-temp-list 
                  (lambda (a b)
                    ;; Small hack: Don't sort on stuff with colons
                    ;; because it could be a reference to a remote
                    ;; machine (this typically appears when browsing
                    ;; to root).
                    (if (and (not (string-match ":" b)) (not (string-match ":" a)))
                        (time-less-p
                         (sixth (file-attributes (concat ido-current-directory b)))
                         (sixth (file-attributes (concat ido-current-directory a))))
                      nil)))))
  (ido-to-end ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))


(provide 'setup-idomode)
