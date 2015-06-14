;; mew configs
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; optional setup ('Read Mail' menu):
(setq read-mail-command 'mew)

;; optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(add-hook 'mew-message-mode-hook
          (lambda ()
            (local-set-key (kbd "O") 'mew-browse-url-at-point)))

(provide 'microamp-mail)
