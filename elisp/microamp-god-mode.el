(require 'god-mode)

(god-mode-all) ;; god-mode enabled on startup

(global-set-key (kbd "<escape>") 'god-mode-all)

;; no god in the following major modes
(add-to-list 'god-exempt-major-modes 'compilation-mode)
(add-to-list 'god-exempt-major-modes 'debugger-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'eshell-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'mew-mode)
(add-to-list 'god-exempt-major-modes 'mew-summary-mode)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-mode-all)

(add-hook 'god-mode-enabled-hook
          (lambda ()
            (set-cursor-color "gray60")
            (set-face-foreground 'mode-line "gray60")))

(add-hook 'god-mode-disabled-hook
          (lambda ()
            (set-cursor-color "#fdf4c1")
            (set-face-foreground 'mode-line "#fdf4c1")))

(provide 'microamp-god-mode)
