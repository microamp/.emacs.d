(require 'git-timemachine)
(require 'magit-gitflow)

;; magit keybindings
(define-key global-map (kbd "C-c m s") 'magit-status)
(define-key global-map (kbd "C-c m l") 'magit-log)
(define-key global-map (kbd "C-c m b m") 'magit-branch-manager)
(define-key global-map (kbd "C-c m f l") 'magit-file-log)
(define-key global-map (kbd "C-c m b l") 'magit-blame-mode)
(define-key global-map (kbd "C-c m a") 'vc-annotate)
(define-key global-map (kbd "C-c m t") 'git-timemachine)

;; scroll to top when magit section is expanded
(defun magit-toggle-scroll-to-top () (recenter-top-bottom 0))
(advice-add 'magit-toggle-section :after #'magit-toggle-scroll-to-top)

;; turn on gitflow
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; turn off auto-revert
(setq magit-auto-revert-mode nil)

;; magit version: 1.4.0
(setq magit-last-seen-setup-instructions "1.4.0")

;; hooks for unbiding C-f and binding C-S-f
(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-f") 'magit-key-mode-popup-gitflow)))

(add-hook 'magit-mode-hook
          (lambda()
            (define-key magit-gitflow-mode-map (kbd "C-f") nil)))

(provide 'microamp-magit)
