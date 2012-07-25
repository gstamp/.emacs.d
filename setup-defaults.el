;; Some sane defaults

;; Save my place so that when I reopen a file I go back to the same place
(setq-default save-place t)

;; By default, Emacs inserts tabs in place of multiple spaces when it
;; formats a region. We want spaces.
(setq-default intent-tabs-mode nil)

;; Indenting defaults
(setq nxml-child-indent 4)
(setq lua-indent-level 4)

;; Allow emacs to access the x clipboard
(setq x-select-enable-clipboard t)

;;  The variable vc-follow-symlinks controls what Emacs does if you
;;  try to visit a symbolic link pointing to a version-controlled
;;  file.
(setq vc-follow-symlinks t)

;; flash instead of ding
(setq visible-bell t)

;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; Put backups in ~/.emacsbackup/ and control how many versions are kept
(setq backup-directory-alist
      (list (cons ".*" (expand-file-name "~/.emacsbackup/")))) 
(setq version-control t)                ; backup multiple versions
(setq delete-old-versions t)            ; delete the older ones
(setq kept-new-versions 10)             ; keep x new ones
(setq kept-old-versions 3)              ; keep x old ones

;; TODO: Not sure this is actually doing anything
;; Sets up whitepsace.
;;  trailing - no trailing whitespace and end of line
;;  lines - lines whose have columns beyond ‘whitespace-line-column’
;;          are highlighted via faces.
;;  tabs - TABs are visualized via faces.
(setq whitespace-style '(trailing lines tabs)
      whitespace-line-column 80)        ; no trailing space or tabs

;; Part of bookmarks plugin.  Only highlight bookmarks on fringe.
(setq bm-highlight-style 'bm-highlight-only-fringe)

;; ignore slime complaining about the version mismatch (& set default
;; ports)
(setq slime-protocol-version 'ignore)
(defvar slime-port 4005)
(defvar durendal-port 4005)

;; defaults to iso-8895-1  encoding otherwise.
(setq slime-net-coding-system 'utf-8-unix)

;; stop slime giving me annoying messages
(setq font-lock-verbose nil)

;; gerkin config
(setq feature-default-language "en")

;; Display/redraw defaults
(setq set-cursor-type 'box)
(set-cursor-color "yellow")

;; Can't see a good reason not to enable these commands
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Emacs starter kit turns on flyspell by default but I prefer it off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Emacs starter kit also turns on idle-highlight-mode.  Not a fan.
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

;; Colour org mode source code
(setq org-src-fontify-natively t)
;; When a task is finished log when it's done
(setq org-log-done 'time)

(setq is-mac (equal system-type 'darwin))

(when is-mac
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
    (set-face-attribute 'default nil :font "Monaco-16")
    ))

;; Add rainbow delimiters to all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; Alternatively it can be hooked up to specific modes:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; ..or globally..
;; (global-rainbow-delimiters-mode)



(provide 'setup-defaults)
