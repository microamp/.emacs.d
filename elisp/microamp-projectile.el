(require 'projectile)
(require 'helm-projectile)
(require 'swiper-helm)

(autoload 'ivy-read "ivy")

;; projectile on globally
(projectile-global-mode)

;; indexing method
(setq projectile-indexing-method 'native)

;; caching
(setq projectile-enable-caching t)

;; projectile-find-file ("C-c p f")

(define-key global-map (kbd "C-c p s f") 'find-file-in-project)

;; helm-projectile-grep ("C-c p s g")
;; helm-projectile-ag ("C-c p s s")

;; projectile-dired ("C-c p D")

;; C-c p C-h to view all keybindings

;; helm integration on
(helm-projectile-on)

(provide 'microamp-projectile)
