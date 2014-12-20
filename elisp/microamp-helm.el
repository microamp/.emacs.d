(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
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
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; helm-show-kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm-find ("C-c h /") (use prefix "C-u" to choose a dir)

;; helm-do-grep ("C-c h g /") (use prefix "C-u" to do recursive grep)
(global-set-key (kbd "C-c h g /") 'helm-do-grep)

;; helm-find-files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; helm-ff-do-grep, live grep in Helm ("C-x C-f" -> "C-s" to search (use prefix "C-u C-s" to do recursive grep))
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; helm-semantic-or-imenu ("C-c h i" to invoke)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

;; helm-resume ("C-c h b")

(provide 'microamp-helm)
