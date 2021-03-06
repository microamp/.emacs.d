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
    deft
    dired+
    dired-rainbow
    dired-toggle-sudo
    ein
    elisp-slime-nav
    elixir-mode
    emms
    emms-mode-line-cycle
    ensime
    epc
    erc-hl-nicks
    eredis
    exec-path-from-shell
    fancy-battery
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
    go-playground
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
    helm-emms
    helm-projectile
    helm-pt
    helm-pydoc
    helm-spotify
    help-fns+
    highlight
    highlight-indentation
    highlight-parentheses
    hl-todo
    howdoi
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
    password-generator
    magit
    magit-gitflow
    markdown-mode
    merlin
    mew
    monky
    mpc
    multi-eshell
    multiple-cursors
    neotree
    nodejs-repl
    nyan-prompt
    ox-reveal
    paradox
    powerline
    projectile
    py-autopep8
    racket-mode
    rcirc-color
    rcirc-notify
    restclient
    ruby-mode
    scala-mode2
    smartparens
    sml-mode
    sunshine
    swiper-helm
    tuareg
    twittering-mode
    utop
    w3m
    wiki-summary
    yaml-mode
    yasnippet
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
(require 'company)
(require 'deft)
(require 'dired-rainbow)
(require 'direx)
(require 'markdown-mode)
(require 'multiple-cursors)
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
(setq debug-on-error nil)

(blink-cursor-mode t) ;; make cursor blink

(global-auto-revert-mode t) ;; refresh buffers when changed on disk

(global-hl-todo-mode t) ;; highlight TODO, FIXME, etc.

(add-hook 'java-mode-hook 'hl-todo-mode)

(add-hook 'sql-mode-hook 'hl-todo-mode)

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
        minutes
        " "))
(display-time-mode 1)

;; display battery life
(add-hook 'after-init-hook #'fancy-battery-mode)

;; default font
(set-default-font
 (if on-os-x? "Source code pro-10" "Terminus-8"))

;; turn off visible and ring bells
(setq visible-bell nil
      ring-bell-function 'ignore)

;; increase auto-save interval for org-mode
(add-hook 'org-mode-hook (lambda ()
                           (interactive)
                           (set (make-local-variable 'auto-save-interval) 60)))

;; show details in dired buffers
(add-hook 'dired-mode-hook
          '(lambda ()
             (dired-hide-details-mode -1)))

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
  ("N" highlight-symbol-prev)
  ("q" highlight-symbol-query-replace))

(global-set-key (kbd "<f10>") 'hydra-highlight-symbol/body)

;; dired-rainbow settings
(dired-rainbow-define media "#BC8383" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
(dired-rainbow-define elisp "#DFAF8F" ("clj" "el" "ex" "exs" "go" "js" "py"))

;; scroll settings
(setq-default scroll-step 1
              scroll-margin 5
              scroll-conservatively 10000
              redisplay-dont-pause t)

;; gc settings
(setq-default gc-cons-percentage 0.2)

;; turn on font-lock mode to colour text in certain modes
(global-font-lock-mode t)

;; make sure spaces are used when indenting code
(setq-default indent-tabs-mode nil)

;; doc-view-mode: continuous navigation via C-n/C-p
(setq-default doc-view-continuous 1)

;; highlight current line
(global-hl-line-mode t)

;; programming mode hooks
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'which-func-mode)

;; snippets via yasnippet
(define-key company-active-map [tab] nil) ;; disable TAB in company to avoid conflict with yasnippet
(setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet/snippets"))
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; remove trailing whitespaces
(add-hook 'prog-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; highlight matching parens
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; map C-c v to eval-buffer
(define-key global-map (kbd "C-c v") 'eval-buffer)

;; map RET to newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; map keybindings for switching to next/previous windows
(define-key global-map (kbd "C-x n") 'other-window)
(define-key global-map (kbd "C-x p") 'previous-multiframe-window)

;; alternative bindings to C-x 0, C-x 1, C-x 2 and C-x 3
(define-key global-map (kbd "C-x q") 'delete-window)
(define-key global-map (kbd "C-x l") 'delete-other-windows)

;; focus the new window after split
(defun split-vertical ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-horizontal ()
  (interactive)
  (split-window-below)
  (other-window 1))

(define-key global-map (kbd "C-x w") 'split-horizontal)
(define-key global-map (kbd "C-x -") 'split-horizontal)

(define-key global-map (kbd "C-x v") 'split-vertical)
(define-key global-map (kbd "C-x |") 'split-vertical)

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

;; markdown keybindings
(define-key markdown-mode-map (kbd "M-n") 'vi-style-c-e)
(define-key markdown-mode-map (kbd "M-p") 'vi-style-c-y)

;; truncate lines for markdown mode
(add-hook 'markdown-mode-hook (lambda () (setq truncate-lines t)))

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

;; weather forecast
(setq sunshine-location "Auckland, NZ")

;; helm-dash integration
(setq
 emacs-lisp-docsets '("Emacs Lisp")
 html-docsets '("Bootstrap 3" "HTML")
 css-docsets '("Bootstrap 3" "CSS")
 shell-docsets '("Bash")
 lua-docsets '("Lua" "Redis"))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets emacs-lisp-docsets)))
(add-hook 'html-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets html-docsets)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets css-docsets)))
(add-hook 'shell-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets shell-docsets)))
(add-hook 'lua-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets lua-docsets)))

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

;; github-flavoured markdown (for *.md/markdown files)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
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

;; multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-m C-S-c") 'mc/mark-all-like-this)

;; deft settings
(setq deft-extensions '("markdown"
                        "md"
                        "org"
                        "txt"))
(setq deft-directory "~/Dropbox/.deft")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(define-key deft-mode-map (kbd "C-k") 'deft-filter-clear)
(define-key deft-mode-map (kbd "M-q") 'ibuffer-quit)
(advice-add 'deft :after 'deft-filter-clear)
(advice-add 'deft :after 'deft-refresh)

;; pretty symbols
(global-prettify-symbols-mode 1)
(setq prettify-symbols-alist '(("lambda" . 955)))

;; howdoi keybindings
(define-key global-map (kbd "C-x M-o") 'howdoi-query)

;; load custom elisp libraries
(add-to-list 'load-path custom-lib-dir)
(load-library "microamp-chat")
(load-library "microamp-clojure")
(load-library "microamp-colours")
(load-library "microamp-emms")
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
(load-library "microamp-scala")
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
 '(auto-save-default nil)
 '(deft-auto-save-interval 0.0)
 '(emms-mode-line-cycle-additional-space-num 4)
 '(emms-mode-line-cycle-current-title-function
   (lambda nil
     (let
         ((track
           (emms-playlist-current-selected-track)))
       (cl-case
           (emms-track-type track)
         ((streamlist)
          (let
              ((stream-name
                (emms-stream-name
                 (emms-track-get track
                                 (quote metadata)))))
            (if stream-name stream-name
              (emms-track-description track))))
         ((url)
          (emms-track-description track))
         (t
          (file-name-nondirectory
           (emms-track-description track)))))))
 '(emms-mode-line-cycle-max-width 15)
 '(emms-mode-line-cycle-use-icon-p t)
 '(emms-mode-line-format " [%s]")
 '(emms-source-file-default-directory "~/Music/")
 '(fancy-battery-show-percentage nil)
 '(god-mod-alist (quote ((nil . "C-M-"))))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-log-section-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-log-select-arguments (quote ("--graph" "--color" "--decorate")))
 '(paradox-github-token t)
 '(sunshine-units (quote metric)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#FB4933" :foreground "#FFFFC8"))))
 '(avy-lead-face-0 ((t (:background "#076678" :foreground "#FFFFC8"))))
 '(avy-lead-face-1 ((t (:background "#B57614" :foreground "#FFFFC8"))))
 '(avy-lead-face-2 ((t (:background "#A89984" :foreground "white"))))
 '(ensime-implicit-highlight ((t (:underline "#B57614"))))
 '(fancy-battery-charging ((t (:foreground "olive drab"))))
 '(fancy-battery-critical ((t (:foreground "dark red"))))
 '(fancy-battery-discharging ((t (:foreground "rosy brown"))))
 '(flymake-warnline ((t (:background "#4E3D45"))))
 '(go-direx-package ((t (:foreground "#FB4933" :weight bold))))
 '(highlight-symbol-face ((t (:background "#FB4933"))))
 '(mu4e-header-highlight-face ((t (:inherit region :weight bold))))
 '(neo-dir-link-face ((t (:foreground "#A89984"))))
 '(neo-expand-btn-face ((t (:foreground "#FB4933"))))
 '(neo-file-link-face ((t (:foreground "#FFFFC8"))))
 '(region ((t (:background "#427B58"))))
 '(which-func ((t (:foreground "#83A598")))))
