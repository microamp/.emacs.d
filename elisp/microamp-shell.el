(require 'bash-completion)

;; turn on auto completion for bash
(bash-completion-setup)

;; hide password in shell mode
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; close *Completions* buffer when command selected
(defun delete-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)

(add-hook 'comint-preoutput-filter-functions
          'delete-completion-window-buffer)

;; clear when C-l
(defun clear-shell ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "C-l") 'clear-shell)))

;; map C-x s to open (multi-)eshell
;(define-key global-map (kbd "C-x s") 'multi-eshell)
;(define-key global-map (kbd "M-SPC") 'multi-eshell-switch)
(define-key global-map (kbd "C-x s") 'eshell)
(define-key global-map (kbd "M-SPC") 'eshell)

;; preserve M-n/M-p
;(add-hook 'eshell-mode-hook
;          (lambda () (local-set-key (kbd "M-n") 'comint-next-input)))
;(add-hook 'eshell-mode-hook
;          (lambda () (local-set-key (kbd "M-p") 'comint-previous-input)))
(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'eshell-previous-matching-input-from-input)))
(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'eshell-next-matching-input-from-input)))

(provide 'microamp-shell)
