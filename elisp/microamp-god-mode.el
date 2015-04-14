(require 'god-mode)

(global-set-key (kbd "<C-return>") 'god-mode-all)

;; no god in the following major modes
(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'eshell-mode)

(define-key god-local-mode-map (kbd ".") 'repeat)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(provide 'microamp-god-mode)
