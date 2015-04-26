(require 'color-theme)
(require 'color-theme-buffer-local)
(require 'load-theme-buffer-local)

;; colour theme
;(load-theme 'zenburn t)
(load-theme 'darktooth t)

;; custom colours
(custom-set-faces
 '(hl-line ((t (:background "gray25"))))
 '(linum ((t (:background "black" :foreground "gray50"))))
 '(region ((t (:background "#9ece9e" :foreground "#1f1f1f"))))
 '(show-paren-match ((t (:background "#1f1f1f" :foreground "#9ece9e" :weight bold)))))

(set-cursor-color "gray60") ;; point colour

;; use same theme/font for dired-mode
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

(provide 'microamp-colours)
