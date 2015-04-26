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

;; helm-projectile-grep ("C-c p s g")
;; helm-projectile-ag ("C-c p s s")

;; projectile-dired ("C-c p D")

;; C-c p C-h to view all keybindings

;; helm integration on
(helm-projectile-on)

;; `find-file-in-project` replacing `helm-projectile-find-file`
(define-key projectile-mode-map (kbd "C-c p f") 'find-file-in-project)

(provide 'microamp-projectile)
