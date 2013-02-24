
;; Make ido open file list vertically
(setq ido-max-prospects 60)
(setq ido-max-file-prompt-width 0.8)
(setq ido-max-window-height 30)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(setq ffip-full-paths 't) ; Not really part of ido but works well with
                          ; vertically displayed lists.


(provide 'setup-idomode)
