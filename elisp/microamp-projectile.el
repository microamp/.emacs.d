(require 'projectile)
(require 'helm-projectile)

;; projectile on globally
(projectile-global-mode)

;; indexing method
(setq projectile-indexing-method 'native)

;; caching
(setq projectile-enable-caching t)

;; projectile-find-file ("C-c p f")

;; helm-projectile-grep ("C-c p s g")

;; C-c p C-h to view all keybindings

;; helm integration on
(helm-projectile-on)

(provide 'microamp-projectile)
