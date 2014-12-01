(require 'color-theme)
(require 'color-theme-buffer-local)
(require 'load-theme-buffer-local)

;; colour theme
(load-theme 'zenburn t)

;; custom colours
(custom-set-faces
 '(hl-line ((t (:background "gray30"))))
 '(linum ((t (:background "black" :foreground "gray50")))))

(set-cursor-color "#777777")  ;; cursor colour

;; use same theme/font for dired-mode
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

;; use 'tango' theme for dired/dired+
;(add-hook 'dired-mode-hook
;          (lambda nil (load-theme-buffer-local 'tango (current-buffer))))

;; use 'tango-dark' theme for mew
;(add-hook 'mew-summary-mode-hook
;          (lambda nil (load-theme-buffer-local 'tango-dark (current-buffer))))
;(add-hook 'mew-message-mode-hook
;          (lambda nil (load-theme-buffer-local 'tango-dark (current-buffer))))
;(add-hook 'mew-draft-mode-hook
;          (lambda nil (load-theme-buffer-local 'tango-dark (current-buffer))))

;; use 'tango' theme for mew
;(add-hook 'mpc-mode-hook
;          (lambda nil (load-theme-buffer-local 'tango (current-buffer))))

(provide 'microamp-colours)
