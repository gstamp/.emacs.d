
;; You'll need to:
;;  gem install terminal-notifier
;; for this to work and be on OS/X 10.8
(defun osx-notify (title message)
  "Shows a MESSAGE with TITLE using OSX Mountain Lion notifications"
  (shell-command-to-string
   (format "terminal-notifier -title '%s' -message '%s'"
           title message)))

(defun osx-notify-erc-hook (match-type nick message)
  "Notify when nick is mentioned"
  (when (eq match-type 'current-nick)
    (osx-notify (buffer-name (current-buffer)) message)))


(when (equal system-type 'darwin)
  (progn
    ;; map meta to the command key on mac
    (setq mac-option-key-is-meta nil)
    (setq mac-left-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)

    ;; I like the right option key to be control because there's no
    ;; right control key on the mac
    (setq mac-right-option-modifier 'control)

    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)

    ;; keybinding to toggle full screen mode
    (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

    ;; Move to trash when deleting stuff
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs")

    ;; mac friendly font
    (if window-system
        (set-face-attribute 'default nil :font "Monaco-16"))

    (add-hook 'erc-text-matched-hook 'osx-notify-erc-hook)))

(provide 'setup-notifications)
