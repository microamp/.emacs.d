(require 'cl)

;; packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar package-list
  '(alchemist
    auto-complete
    bash-completion
    bookmark+
    cider
    clojure-mode
    color-theme
    color-theme-buffer-local
    company
    dired+
    dired-rainbow
    dired-toggle-sudo
    ein
    elixir-mode
    epc
    erc-hl-nicks
    fill-column-indicator
    flymake-cursor
    flymake-easy
    flymake-python-pyflakes
    go-autocomplete
    go-eldoc
    go-mode
    go-play
    google-this
    haskell-mode
    helm
    helm-descbinds
    helm-projectile
    helm-pydoc
    help-fns+
    highlight
    highlight-parentheses
    htmlize
    hy-mode
    ;ido-load-library
    ipython
    jabber
    jedi
    ;jedi-direx
    lfe-mode
    load-theme-buffer-local
    magit
    merlin
    mew
    monky
    mpc
    multi-eshell
    nodejs-repl
    org
    ox-reveal
    paredit
    projectile
    racket-mode
    rainbow-delimiters
    rcirc-color
    rcirc-notify
    restclient
    ruby-mode
    sml-mode
    ;starter-kit
    ;starter-kit-eshell
    ;starter-kit-lisp
    tuareg
    twittering-mode
    utop
    w3m
    yaml-mode
    zenburn-theme)
  "List of packages needs to be upgraded/installed at launch")

(defun package-missing? ()
  (dolist (p package-list)
    (unless (package-installed-p p)
      (return t))))

(when (package-missing?)
  (package-refresh-contents)
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'dired+)
(require 'dired-rainbow)
(require 'helm-config)
(require 'highlight-parentheses)
;(require 'ido)
(require 'rainbow-delimiters)

(setq emacs-dir "~/.emacs.d")
(setq custom-lib-dir "elisp")

(menu-bar-mode -1) ;; hide menu bar
(tool-bar-mode -1) ;; hide tool bar

(blink-cursor-mode t) ;; make cursor blink

(set-default-font "Terminus-8") ;; default font

(setq visible-bell nil) ;; turn visible bell off

(add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode -1))) ;; show details

;; /usr/local/bin added to exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; always use utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; browse url using firefox
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; dired+ for reusing dired buffers
(toggle-diredp-find-file-reuse-dir t)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; dired-rainbow settings
(dired-rainbow-define media "#BC8383" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
(dired-rainbow-define elisp "#DFAF8F" ("el"))
(dired-rainbow-define python "#F0DFAF" ("py"))
(dired-rainbow-define go "#F0DFAF" ("go"))
(dired-rainbow-define javascript "#F0DFAF" ("js"))
(dired-rainbow-define clojure "#F0DFAF" ("clj"))
(dired-rainbow-define elixir "#F0DFAF" ("exs" "ex"))

;; disable automatic scrolling/re-centering
(setq-default scroll-step 1
              scroll-margin 0)

;; turn on font-lock mode to colour text in certain modes
(global-font-lock-mode t)

;; make sure spaces are used when indenting code
(setq-default indent-tabs-mode nil)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; doc-view-mode: continuous navigation via C-n/C-p
(setq-default doc-view-continuous 1)

;; highlight current line
(global-hl-line-mode t)

;; ido
;(ido-mode nil)

;; electric-pair
(electric-pair-mode t)

;; programming mode hooks
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; highlight-parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; map C-c v to eval-buffer
(define-key global-map (kbd "C-c v") 'eval-buffer)

;; map RET to newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; map keybindings for switching to next/previous windows
(define-key global-map (kbd "M-N") 'other-window)
(define-key global-map (kbd "M-P") 'previous-multiframe-window)
(define-key global-map (kbd "C-x n") 'other-window)
(define-key global-map (kbd "C-x p") 'previous-multiframe-window)

;; map C-. to set-mark-command
(define-key global-map (kbd "C-.") 'set-mark-command)

;; alternative bindings to C-x 0, C-x 1, C-x 2 and C-x 3
(define-key global-map (kbd "C-x q") 'delete-window)
(define-key global-map (kbd "C-x l") 'delete-other-windows)

;; focus the new window after split
(global-set-key "\C-xw" (lambda ()
                          (interactive)
                          (split-window-below)
                          (other-window 1)))
(global-set-key "\C-xv" (lambda ()
                          (interactive)
                          (split-window-right)
                          (other-window 1)))

;; magit keybindings
(define-key global-map (kbd "C-c m s") 'magit-status)
(define-key global-map (kbd "C-c m l") 'magit-log)
(define-key global-map (kbd "C-c m b m") 'magit-branch-manager)

;; scroll to top when magit section is expanded
(defun magit-toggle-scroll-to-top () (recenter-top-bottom 0))
(advice-add 'magit-toggle-section :after #'magit-toggle-scroll-to-top)

;; vi-style C-e/C-y
(defun vi-style-c-e (n)
  (interactive "p")
  (scroll-up n))

(defun vi-style-c-y (n)
  (interactive "p")
  (scroll-down n))

(global-set-key "\M-n" 'vi-style-c-e)
(global-set-key "\M-p" 'vi-style-c-y)

;; w3m keybindings
(add-hook 'w3m-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'w3m-scroll-up)))
(add-hook 'w3m-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'w3m-scroll-down)))

;; set tab width for go mode
(add-hook 'go-mode-hook
  (function
   (lambda ()
     (setq tab-width 2))))

;; load custom elisp libraries
(add-to-list 'load-path (concat emacs-dir "/" custom-lib-dir))
(load-library "microamp-chat")
(load-library "microamp-colours")
(load-library "microamp-elixir")
(load-library "microamp-haskell")
(load-library "microamp-helm")
(load-library "microamp-lisp")
(load-library "microamp-mail")
(load-library "microamp-mode-line")
(load-library "microamp-ocaml")
(load-library "microamp-org")
(load-library "microamp-presentation")
(load-library "microamp-projectile")
(load-library "microamp-python")
(load-library "microamp-shell")
