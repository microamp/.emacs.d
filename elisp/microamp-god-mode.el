(require 'god-mode)

(global-set-key (kbd "<backtab>") 'god-mode-all)

;; no god in the following major modes
(add-to-list 'god-exempt-major-modes 'compilation-mode)
(add-to-list 'god-exempt-major-modes 'debugger-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'eshell-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'mew-message-mode)
(add-to-list 'god-exempt-major-modes 'mew-mode)
(add-to-list 'god-exempt-major-modes 'mew-summary-mode)
(add-to-list 'god-exempt-major-modes 'special-mode)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-mode-all) ;; deactivate
(define-key god-local-mode-map (kbd "<backtab>") 'ignore) ;; activate

(defun set-colour-god-mode-on ()
  (interactive)
  (set-cursor-color "gray60")
  (set-face-foreground 'mode-line "gray60")
  (set-face-background 'hl-line "gray20"))

(defun set-colour-god-mode-off ()
  (interactive)
  (set-cursor-color "#fdf4c1")
  (set-face-foreground 'mode-line "#fdf4c1")
  (set-face-background 'hl-line "#2b3c44"))

(god-mode-all) ;; god-mode enabled on startup
(set-colour-god-mode-on)

(add-hook 'god-mode-enabled-hook 'set-colour-god-mode-on)
(add-hook 'god-mode-disabled-hook 'set-colour-god-mode-off)

(provide 'microamp-god-mode)
