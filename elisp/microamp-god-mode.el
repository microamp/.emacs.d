(require 'god-mode)

;; no god in the following major modes
(add-to-list 'god-exempt-major-modes 'compilation-mode)
(add-to-list 'god-exempt-major-modes 'debugger-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'eshell-mode)
(add-to-list 'god-exempt-major-modes 'eww-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'mew-message-mode)
(add-to-list 'god-exempt-major-modes 'mew-mode)
(add-to-list 'god-exempt-major-modes 'mew-summary-mode)
(add-to-list 'god-exempt-major-modes 'special-mode)
(add-to-list 'god-exempt-major-modes 'vc-annotate-mode)

(define-key global-map (kbd "<escape>") 'god-mode) ;; activate (global keymap)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-mode) ;; deactivate
(define-key god-local-mode-map (kbd "<escape>") 'ignore) ;; activate

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hollow
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;;(god-mode) ;; activated on startup

(provide 'microamp-god-mode)
