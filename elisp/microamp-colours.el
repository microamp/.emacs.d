(require 'color-theme)
(require 'color-theme-buffer-local)
(require 'load-theme-buffer-local)

;; colour theme
;(load-theme 'zenburn t)
(load-theme 'darktooth t)

(set-cursor-color "#fdf4c1") ;; point colour

;; use same theme/font for dired-mode
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

(provide 'microamp-colours)
