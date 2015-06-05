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
    avy
    bash-completion
    bookmark+
    calfw
    cider
    clojure-mode
    color-theme
    color-theme-buffer-local
    company
    darktooth-theme
    dired+
    dired-rainbow
    dired-toggle-sudo
    ein
    elixir-mode
    epc
    erc-hl-nicks
    fill-column-indicator
    find-file-in-project
    flymake-cursor
    flymake-easy
    flymake-jslint
    flymake-python-pyflakes
    geiser
    git-timemachine
    go-autocomplete
    go-eldoc
    go-mode
    go-play
    god-mode
    google-this
    guide-key
    haskell-mode
    helm
    helm-ag
    helm-descbinds
    helm-projectile
    helm-pt
    helm-pydoc
    help-fns+
    highlight
    highlight-parentheses
    htmlize
    hy-mode
    ipython
    jabber
    jedi
    js2-mode
    lfe-mode
    load-theme-buffer-local
    magit
    magit-gitflow
    merlin
    mew
    monky
    mpc
    multi-eshell
    nodejs-repl
    nyan-prompt
    org
    ox-reveal
    paradox
    powerline
    projectile
    racket-mode
    rcirc-color
    rcirc-notify
    restclient
    ruby-mode
    smartparens
    sml-mode
    swiper-helm
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

(require 'auto-complete-config)
(require 'calfw)
(require 'dired+)
(require 'dired-rainbow)
(require 'guide-key)
(require 'helm-config)
(require 'highlight-parentheses)
(require 'paradox)
(require 'paren)

(setq emacs-dir "~/.emacs.d")
(setq custom-lib-dir "elisp")

(setq on-os-x? (equal system-type 'darwin))

(menu-bar-mode -1) ;; hide menu bar
(tool-bar-mode -1) ;; hide tool bar

(blink-cursor-mode t) ;; make cursor blink

(global-auto-revert-mode t) ;; refresh buffers when changed on disk

(ac-config-default) ;; auto-completion on!
(setq ac-auto-show-menu nil) ;; but with no popup!

;; default font
(set-default-font
 (if on-os-x? "Source code pro-10" "Terminus-8"))

;; fix messed up keybindings if on os x (emacs-mac-port)
(when on-os-x?
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-function-modifier 'hyper))

(setq visible-bell nil) ;; turn visible bell off
(setq ring-bell-function 'ignore) ;; turn ring bell off

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

;; show github stars in paradox
(setq paradox-github-token t)
(define-key global-map (kbd "C-x C-p") (lambda ()
                                         (interactive)
                                         (paradox-list-packages nil)))

;; dired-rainbow settings
(dired-rainbow-define media "#BC8383" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
(dired-rainbow-define elisp "#DFAF8F" ("clj" "el" "ex" "exs" "go" "js" "py"))

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

;; programming mode hooks
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'which-func-mode)

;; highlight matching parens
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

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

;; vi-style C-e/C-y
(defun vi-style-c-e (n)
  (interactive "p")
  (scroll-up n))

(defun vi-style-c-y (n)
  (interactive "p")
  (scroll-down n))

(global-set-key "\M-n" 'vi-style-c-e)
(global-set-key "\M-p" 'vi-style-c-y)

;; shortcut to switch to *scratch* buffer
(define-key global-map (kbd "C-x s") (lambda ()
                                       (interactive)
                                       (switch-to-buffer "*scratch*")))

;; w3m keybindings
(add-hook 'w3m-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'w3m-scroll-up)))
(add-hook 'w3m-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'w3m-scroll-down)))

;; avy shortcuts
(global-set-key (kbd "C-x g l") 'avy-goto-line)
(global-set-key (kbd "C-x g w") 'avy-goto-word-0)

;; set tab width for go mode
(add-hook 'go-mode-hook
  (function
   (lambda ()
     (setq tab-width 2))))

;; guide-key settings
(setq guide-key/guide-key-sequence '("C-x"
                                     "C-c"
                                     "C-h"))
(setq guide-key/idle-delay 1.0)
(setq guide-key/recursive-key-sequence-flag t)

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(guide-key-mode 1)

;; load custom elisp libraries
(add-to-list 'load-path (concat emacs-dir "/" custom-lib-dir))
(load-library "microamp-chat")
(load-library "microamp-clojure")
(load-library "microamp-colours")
(load-library "microamp-elixir")
(load-library "microamp-git")
(load-library "microamp-god-mode")
(load-library "microamp-helm")
(load-library "microamp-js")
(load-library "microamp-mew")
(load-library "microamp-mode-line")
(load-library "microamp-ocaml")
(load-library "microamp-org")
(load-library "microamp-presentation")
(load-library "microamp-projectile")
(load-library "microamp-python")
(load-library "microamp-racket")
(load-library "microamp-shell")
(load-library "microamp-smartparens")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(god-mod-alist (quote ((nil . "C-M-")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#FB4933" :foreground "#FFFFC8"))))
 '(avy-lead-face-0 ((t (:background "#076678" :foreground "#FFFFC8"))))
 '(avy-lead-face-1 ((t (:background "#B57614" :foreground "#FFFFC8"))))
 '(linum ((t (:background "black" :foreground "gray50"))))
 '(region ((t (:background "#9ece9e" :foreground "#1f1f1f"))))
 '(show-paren-match ((t (:background "#1f1f1f" :foreground "#9ece9e" :weight bold))))
 '(which-func ((t (:foreground "#83A598")))))
