(require 'helm-projectile)
(require 'helm-pt)
(require 'projectile)
(require 'swiper-helm)

(autoload 'ivy-read "ivy")

;; projectile on globally
(projectile-global-mode)

;; indexing method
(setq projectile-indexing-method 'native)

;; caching
(setq projectile-enable-caching t)

;; helm-projectile-grep ("C-c p s g")
;; helm-projectile-ag ("C-c p s s")

;; projectile-dired ("C-c p D")

;; C-c p C-h to view all keybindings

;; helm integration on
(helm-projectile-on)

;; `find-file-in-project` by default
(defun ff-in-p ()
  (interactive)
  (setq ivy-height (/ (frame-height) 3))
  (ffip))

;; custom keybindings for projectile
(define-key projectile-mode-map (kbd "C-c p f") 'helm-projectile-find-file)
(define-key projectile-mode-map (kbd "C-c p s f") 'ff-in-p)
(define-key projectile-mode-map (kbd "C-c p s p") 'helm-projectile-pt)
(define-key projectile-mode-map (kbd "C-c p w") 'projectile-switch-project)

;; neotree integration
(setq projectile-switch-project-action 'neotree-projectile-action)

(provide 'microamp-projectile)
