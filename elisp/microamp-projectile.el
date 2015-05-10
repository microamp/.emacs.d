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

;; `find-file-in-project` by default
(defun ff-in-p ()
  (interactive)
  (setq ivy-height (/ (frame-height) 3))
  (ffip))

(define-key projectile-mode-map (kbd "C-c p f") 'ff-in-p)

(define-key projectile-mode-map (kbd "C-c p s f") 'helm-projectile-find-file)

(provide 'microamp-projectile)
