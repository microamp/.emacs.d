(require 'color-theme)
(require 'color-theme-buffer-local)
(require 'load-theme-buffer-local)

;; colour theme
;(load-theme 'zenburn t)
(load-theme 'darktooth t)

(set-cursor-color "#fdf4c1") ;; point colour

;; use same theme/font for dired-mode
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

;; use zenburn theme for eww/git-timemachine/vc-annotate
(add-hook 'eww-mode-hook
          (lambda nil (load-theme-buffer-local 'zenburn (current-buffer))))
(add-hook 'git-timemachine-mode-hook
          (lambda nil (load-theme-buffer-local 'zenburn (current-buffer))))
(add-hook 'vc-annotate-mode-hook
          (lambda nil (load-theme-buffer-local 'zenburn (current-buffer))))

(provide 'microamp-colours)
