(require 'color-theme)
(require 'color-theme-buffer-local)
(require 'load-theme-buffer-local)

;; colour theme
(load-theme 'zenburn t)

;; custom colours
(custom-set-faces
 '(region ((t (:background "#5d7f8c"))))
 '(hl-line ((t (:background "gray35"))))
 '(linum ((t (:background "black" :foreground "gray50")))))

(set-cursor-color "#999999") ;; point colour

;; use same theme/font for dired-mode
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

(provide 'microamp-colours)
