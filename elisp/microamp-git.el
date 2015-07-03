(require 'git-timemachine)
(require 'magit-gitflow)

;; magit keybindings
(define-key global-map (kbd "C-c m s")
  (lambda ()
    (interactive)
    (delete-other-windows)
    (magit-status nil)))
(define-key global-map (kbd "C-c m l")
  (lambda ()
    (interactive)
    (magit-log-all)
    (delete-other-windows)))
(define-key global-map (kbd "C-c m b m")
  (lambda ()
    (interactive)
    (magit-show-refs-head)
    (delete-other-windows)))
(define-key global-map (kbd "C-c m f l")
  (lambda ()
    (interactive)
    (magit-log-buffer-file)
    (delete-other-windows)))
(define-key global-map (kbd "C-c m b l")
  (lambda ()
    (interactive)
    (magit-blame-mode)
    (delete-other-windows)))
(define-key global-map (kbd "C-c m a")
  (lambda ()
    (interactive)
    (vc-annotate (buffer-file-name) "HEAD")
    (delete-other-windows)))
(define-key global-map (kbd "C-c m t") 'git-timemachine)

;; scroll to top when magit section is expanded
(defun magit-toggle-scroll-to-top ()
  (recenter-top-bottom 0))
(advice-add 'magit-toggle-section :after #'magit-toggle-scroll-to-top)

;; turn on gitflow
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; additional keybindings
(define-key magit-mode-map (kbd "<C-return>") 'magit-checkout)

;; keybindings for gitflow (C-f to C-S-f)
(define-key magit-gitflow-mode-map (kbd "C-f") nil)
(define-key magit-mode-map (kbd "C-S-f") 'magit-gitflow-popup)

(provide 'microamp-magit)
