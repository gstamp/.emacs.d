;; Emac 24 configuration for Glen Stampoultzis
;;
;; See README.md for more details.  The latest version of this
;; configuration can be found at https://github.com/gstamp/.emacs.d
;;  

;; Start by requiring and initializing the package manager.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; This is the list of packages we want installed by default.
(defvar my-packages '(starter-kit 
                      starter-kit-lisp 
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-ruby
                      starter-kit-lisp
                      starter-kit-js
                      clojure-mode
                      align-cljlet
                      cljdoc
                      midje-mode
                      slime
                      solarized-theme
                      yaml-mode
                      bm
                      sgml-mode
                      expand-region
                      elein
                      rainbow-delimiters
                      feature-mode
                      groovy-mode
                      undo-tree
                      projectile
                      markdown-mode
                      graphviz-dot-mode
                      dired-details
                      auto-complete
                      ac-slime
                      highlight-symbol
                      wgrep
                      htmlize
                      )
  "A list of packages to ensure are installed at launch.")

;; Install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; Set up load path
(add-to-list 'load-path dotfiles-dir)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'setup-site-lisp)
(require 'org-confluence)
(require 'setup-defaults)
(require 'setup-grep)
(require 'setup-default-modes)
(require 'custom-defuns)
(require 'setup-keybindings)
(require 'setup-clojure)
(require 'setup-dired)
(require 'setup-javascript)
(require 'setup-completion)
(require 'setup-orgmode)
(require 'setup-idomode)
