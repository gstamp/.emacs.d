;;; tabkey2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tabkey2-emma-without-tabkey2 tabkey2-mode tabkey2)
;;;;;;  "tabkey2" "tabkey2.el" (20872 36638 0 0))
;;; Generated autoloads from tabkey2.el

(let ((loads (get 'tabkey2 'custom-loads))) (if (member '"tabkey2" loads) nil (put 'tabkey2 'custom-loads (cons '"tabkey2" loads))))

(defvar tabkey2-mode nil "\
Non-nil if Tabkey2 mode is enabled.
See the command `tabkey2-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabkey2-mode'.")

(custom-autoload 'tabkey2-mode "tabkey2" nil)

(autoload 'tabkey2-mode "tabkey2" "\
More fun with Tab key number two (completion etc).
This global minor mode by default binds Tab in a way that let you
do completion with Tab in all buffers (where it is possible).

The Tab key is easy to type on your keyboard.  Then why not use
it for completion, something that is very useful?  Shells usually
use Tab for completion so many are used to it.  This was the idea
of Smart Tabs and this is a generalization of that idea.

However in Emacs the Tab key is usually used for indentation.
The idea here is that if Tab has been pressed once for
indentation, then as long as point stays further Tab keys might
as well do completion.

So you kind of do Tab-Tab for first completion (and then just
Tab for further completions as long as point is not moved).

And there is even kind of Tab-Tab-Tab completion: If completion
fails the next completion function will be the one you try with
next Tab. (You get some notification of this, of course.)

See `tabkey2-first' for more information about usage.

Note: If you do not want the Tab-Tab behaviour above, but still
want an easy way to reach the available completion functions,
then you can instead of turning on tabkey2-mode enter this in
your .emacs:

 (global-set-key [f8] 'tabkey2-cycle-completion-functions)

After hitting f8 you will then be in the same state as after the
first in tabkey2-mode.

\(fn &optional ARG)" t nil)

(autoload 'tabkey2-emma-without-tabkey2 "tabkey2" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("tabkey2-pkg.el") (20872 36638 413598
;;;;;;  0))

;;;***

(provide 'tabkey2-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tabkey2-autoloads.el ends here
