
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

(provide 'setup-notifications)
