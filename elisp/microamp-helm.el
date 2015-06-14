(require 'helm)
(require 'helm-config)
(require 'helm-dash)
(require 'helm-eshell)
(require 'helm-descbinds)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(define-key global-map (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; helm-M-x
(define-key global-map (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; helm-show-kill-ring
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;; helm-mini
(define-key global-map (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm-find ("C-c h /") (use prefix "C-u" to choose a dir)

;; helm-do-grep ("C-c h g /") (use prefix "C-u" to do recursive grep)
(define-key global-map (kbd "C-c h g /") 'helm-do-grep)

;; helm-find-files
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

;; helm-ff-do-grep, live grep in Helm ("C-x C-f" -> "C-s" to search (use prefix "C-u C-s" to do recursive grep))
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; helm-semantic-or-imenu ("C-c h i" to invoke)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

;; helm-occur
(define-key global-map (kbd "C-c h o") 'helm-occur)

;; helm-all-mark-rings
(define-key global-map (kbd "C-c h SPC") 'helm-all-mark-rings)

;; helm-regexp ("C-c h r")

;; helm-top ("C-c h t")

;; helm-surfraw ("C-c h s") (NOTE: requires 'surfraw' installed)

;; helm-google-suggest
(define-key global-map (kbd "C-c h g g") 'helm-google-suggest)

;; helm-resume ("C-c h b")

;; helm-eshell-history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; helm-minibuffer-history
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; "C-c h C-h" to list helm bindings
(helm-descbinds-mode)

;; disable header lines
;(setq helm-display-header-line nil)
;(set-face-attribute 'helm-source-header nil :height 0.1)

;; fixed helm window size (approx 1/3 of screen)
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 33)
(setq helm-autoresize-min-height 33)

;(setq helm-split-window-in-side-p t)

;; activate path completion
(setq helm-ff-auto-update-initial-value t)

;; helm-dash settings
(setq helm-dash-all-docsets '("AngularJS"
                              "Bash"
                              "Bootstrap 3"
                              "CSS"
                              "Clojure"
                              "D3JS"
                              "Elixir"
                              "Emacs Lisp"
                              "Erlang"
                              "Go"
                              "HTML"
                              "Haskell"
                              "MongoDB"
                              "OCaml"
                              "PostgreSQL"
                              "Python 2"
                              "Python 3"
                              "Rust"
                              "jQuery")
      helm-dash-docsets-path "~/.docsets"
      helm-dash-min-length 2
      ;;helm-dash-browser-func 'browse-url
      helm-dash-browser-func 'eww)

(defun update-docsets (docsets)
  ;; TODO: run async
  (dolist (docset docsets)
    (let ((underscored (replace-regexp-in-string " " "_" docset)))
      (print (concat "installing/upgrading " underscored "..."))
      (helm-dash-install-docset underscored))))

(define-key global-map (kbd "C-c h d i") 'helm-dash-install-docset)
(define-key global-map (kbd "C-c h d u") (lambda ()
                                    (interactive)
                                    (update-docsets helm-dash-docsets)))
(define-key global-map (kbd "C-c h d d") 'helm-dash-at-point)

(provide 'microamp-helm)
