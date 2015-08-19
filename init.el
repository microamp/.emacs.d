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
    anzu
    auto-complete
    avy
    bash-completion
    bookmark+
    calfw
    cider
    clippy
    clojure-cheatsheet
    clojure-mode
    color-theme
    color-theme-buffer-local
    company
    darktooth-theme
    dired+
    dired-rainbow
    dired-toggle-sudo
    ein
    elisp-slime-nav
    elixir-mode
    epc
    erc-hl-nicks
    eredis
    exec-path-from-shell
    fill-column-indicator
    find-file-in-project
    flymake-cursor
    flymake-easy
    flymake-go
    flymake-jslint
    flymake-python-pyflakes
    geiser
    gh-md
    git-timemachine
    go-autocomplete
    go-direx
    go-eldoc
    go-errcheck
    go-mode
    go-play
    god-mode
    golint
    google-this
    guide-key
    haskell-mode
    highlight-symbol
    helm
    helm-ag
    helm-dash
    helm-descbinds
    helm-projectile
    helm-pt
    helm-pydoc
    helm-spotify
    help-fns+
    highlight
    highlight-indentation
    highlight-parentheses
    hl-todo
    htmlize
    hy-mode
    hydra
    ipython
    jabber
    jedi
    js2-mode
    lfe-mode
    load-theme-buffer-local
    lua-mode
    magit
    magit-gitflow
    markdown-mode
    merlin
    mew
    monky
    mpc
    multi-eshell
    neotree
    nodejs-repl
    nyan-prompt
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

(require 'calfw)
(require 'dired-rainbow)
(require 'direx)
(require 'markdown-mode)
(require 'neotree)
(require 'restclient)

(setq emacs-dot-d (concat (getenv "HOME") "/.emacs.d")
      init-file (concat emacs-dot-d "/init.el")
      custom-lib-dir (concat emacs-dot-d "/elisp"))

(setq on-os-x? (equal system-type 'darwin))

(defun consume-fullscreen (filename)
  "Maximise Emacs' frame once init.el is loaded."
  (when (string-equal filename init-file)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(add-hook 'after-load-functions 'consume-fullscreen)

(when on-os-x?
  ;; import paths from shell
  (exec-path-from-shell-initialize)
  ;; NOTE: in case GOBIN is not included in PATH
  (let ((envs '("PATH" "GOBIN")))
    (exec-path-from-shell-copy-envs envs))
  ;; fix keybindings (emacs-mac-port)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'meta
        mac-right-command-modifier 'super
        mac-function-modifier 'hyper))

(menu-bar-mode -1) ;; hide menu bar
(tool-bar-mode -1) ;; hide tool bar
(scroll-bar-mode -1) ;; no scroll bars

(blink-cursor-mode t) ;; make cursor blink

(global-auto-revert-mode t) ;; refresh buffers when changed on disk

(global-hl-todo-mode t) ;; highlight TODO, FIXME, etc.

;; direx keybindings
(define-key direx:direx-mode-map (kbd "<C-return>")
  (lambda ()
    (interactive)
    (progn
      (direx:maybe-find-item)
      (delete-other-windows))))

;; anzu settings (displaying current match and total matches)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(delete-selection-mode 1) ;; highlighted to be replaced

(ac-config-default) ;; auto-completion on!
(setq ac-auto-show-menu nil) ;; but with no popup!

;; display date and time in mode line (in 24-hour format)
(defun zero-fill (s)
  (format "%02d" (string-to-number s)))

(setq display-time-string-forms
      '((substring year -4)
        "-"
        (zero-fill month)
        "-"
        (zero-fill day)
        " "
        24-hours
        ":"
        minutes))
(display-time-mode 1)

;; default font
(set-default-font
 (if on-os-x? "Source code pro-10" "Terminus-8"))

;; turn off visible and ring bells
(setq visible-bell nil
      ring-bell-function 'ignore)

(add-hook 'dired-mode-hook
          '(lambda ()
             (dired-hide-details-mode -1))) ;; show details

;; /usr/local/bin added to exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; always use utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; neotree settings
(setq neo-theme 'ascii)
(define-key neotree-mode-map (kbd "o") 'neotree-enter)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f7] 'neotree-find)
(global-set-key (kbd "<C-tab>") 'neotree-toggle)

;; browse url using chrome/firefox
(setq chrome-on-os-x "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (if on-os-x? chrome-on-os-x "firefox"))

;; dired+ for reusing dired buffers
(toggle-diredp-find-file-reuse-dir t)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; show github stars in paradox
(setq paradox-github-token t)
(define-key global-map (kbd "C-x C-p")
  (lambda ()
    (interactive)
    (paradox-list-packages nil)))

;; "It looks like you want Clippy in your .emacs.d."
(global-set-key (kbd "C-x M-c f") 'clippy-describe-function)
(global-set-key (kbd "C-x M-c v") 'clippy-describe-variable)

;; highlight-symbol keybindings
(defhydra hydra-highlight-symbol ()
  "Defines Hydra keybindings for highlight-symbol."
  ("<f10>" highlight-symbol)
  ("n" highlight-symbol-next)
  ("p" highlight-symbol-prev)
  ("q" highlight-symbol-query-replace))

(global-set-key (kbd "<f10>") 'hydra-highlight-symbol/body)

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

;; alternative bindings to C-x 0, C-x 1, C-x 2 and C-x 3
(define-key global-map (kbd "C-x q") 'delete-window)
(define-key global-map (kbd "C-x l") 'delete-other-windows)

;; focus the new window after split
(define-key global-map (kbd "C-x w")
  (lambda ()
    (interactive)
    (split-window-below)
    (other-window 1)))
(define-key global-map (kbd "C-x v")
  (lambda ()
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

(define-key global-map (kbd "M-n") 'vi-style-c-e)
(define-key global-map (kbd "M-p") 'vi-style-c-y)

;; shortcut to switch to *scratch* buffer
(define-key global-map (kbd "C-x s")
  (lambda ()
    (interactive)
    (switch-to-buffer "*scratch*")))

;; w3m keybindings
(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'w3m-scroll-up)))
(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'w3m-scroll-down)))

;; calfw alias
(defalias 'cfw 'cfw:open-calendar-buffer)

;; avy shortcuts
(define-key global-map (kbd "C-x g l") 'avy-goto-line)
(define-key global-map (kbd "C-x g w") 'avy-goto-word-0)

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

;; eww keybindings
(defun set-eww-keybindings ()
  (local-unset-key (kbd "M-n"))
  (local-unset-key (kbd "M-p"))
  (local-set-key (kbd "j") 'eww-next-bookmark)
  (local-set-key (kbd "k") 'eww-previous-bookmark)
  (local-set-key (kbd "M-n") 'vi-style-c-e)
  (local-set-key (kbd "M-p") 'vi-style-c-y))
(add-hook 'eww-mode-hook 'set-eww-keybindings)

;; helm-dash integration
(setq
 emacs-lisp-docsets '("Emacs Lisp")
 html-docsets '("Bootstrap 3" "HTML")
 css-docsets '("Bootstrap 3" "CSS"))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets emacs-lisp-docsets)))
(add-hook 'html-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets html-docsets)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets css-docsets)))

;; set html indentation level to 2
(add-hook 'html-mode-hook
          (lambda ()
            (interactive)
            (set (make-local-variable 'sgml-basic-offset 2))))

;; activate eldoc-mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; no external frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; M-. and M-, to jump to definition and back respectively in elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; github-flavoured markdown (for README.md files)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(define-key gfm-mode-map (kbd "C-c r") 'gh-md-render-buffer)

;; additional keybindings for navigation
(defun beginning-of-next-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))

(define-key global-map (kbd "M-]") 'beginning-of-next-defun)
(define-key global-map (kbd "M-[") 'beginning-of-defun)

;; enable restclient-mode for all .http files
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; hydra settings (zoom)
(defhydra hydra-zoom (global-map "C-z")
  "Defines Hydra keybindings for zooming in and out."
  ("n" text-scale-increase "in")
  ("p" text-scale-decrease "out"))

;; hydra settings (restclient)
(defhydra hydra-restclient ()
  "Defines Hydra keybindings for restclient-mode."
  ("c" restclient-http-send-current-stay-in-window)
  ("v" restclient-http-send-current)
  ("n" restclient-jump-next)
  ("p" restclient-jump-prev)
  ("r" restclient-http-send-current-raw)
  ("u" restclient-copy-curl-command)
  ("." restclient-mark-current))

(define-key restclient-mode-map (kbd "C-.") 'hydra-restclient/body)

;; load custom elisp libraries
(add-to-list 'load-path custom-lib-dir)
(load-library "microamp-chat")
(load-library "microamp-clojure")
(load-library "microamp-colours")
(load-library "microamp-elixir")
(load-library "microamp-git")
(load-library "microamp-go")
(load-library "microamp-god-mode")
(load-library "microamp-helm")
(load-library "microamp-js")
(load-library "microamp-mew")
(load-library "microamp-mode-line")
(load-library "microamp-mu4e")
(load-library "microamp-ocaml")
(load-library "microamp-org")
(load-library "microamp-presentation")
(load-library "microamp-projectile")
(load-library "microamp-python")
(load-library "microamp-racket")
(load-library "microamp-redis")
(load-library "microamp-shell")
(load-library "microamp-sql")
(load-library "microamp-smartparens")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(god-mod-alist (quote ((nil . "C-M-")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#FB4933" :foreground "#FFFFC8"))))
 '(avy-lead-face-0 ((t (:background "#076678" :foreground "#FFFFC8"))))
 '(avy-lead-face-1 ((t (:background "#B57614" :foreground "#FFFFC8"))))
 '(avy-lead-face-2 ((t (:background "#A89984" :foreground "white"))))
 '(flymake-warnline ((t (:background "#4E3D45"))))
 '(go-direx-package ((t (:foreground "#FB4933" :weight bold))))
 '(highlight-symbol-face ((t (:background "#FB4933"))))
 '(mu4e-header-highlight-face ((t (:inherit region :weight bold))))
 '(neo-dir-link-face ((t (:foreground "#A89984"))))
 '(neo-expand-btn-face ((t (:foreground "#FB4933"))))
 '(neo-file-link-face ((t (:foreground "#FFFFC8"))))
 '(region ((t (:background "#427B58"))))
 '(which-func ((t (:foreground "#83A598")))))
