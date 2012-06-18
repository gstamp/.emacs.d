;; Emac 24 configuration for Glen Stampoultzis
;;
;; See README.md for more details.  The latest version of this
;; configuration can be found at https://github.com/gstamp/.emacs.d
;;  

;; Start by requiring and initializing the package manager.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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
                      clojure-test-mode
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
                      mark-multiple
                      undo-tree
                      projectile
                      markdown-mode
                      graphviz-dot-mode
                      js2-mode
                      dired-details
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

(require 'setup-defaults)
(require 'setup-grep)
(require 'setup-default-modes)
(require 'custom-defuns)
(require 'setup-keybindings)
(require 'setup-clojure)
(require 'setup-dired)


