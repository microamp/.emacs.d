(require 'emms-mode-line-cycle)
(require 'emms-mode-line-icon)
(require 'emms-setup)

(emms-all)
(emms-default-players)

(emms-mode-line 1)
(emms-playing-time 1)
(emms-mode-line-cycle 1)

(global-set-key (kbd "C-c e p") 'emms-start) ;; (p)lay
(global-set-key (kbd "C-c e s") 'emms-stop) ;; (s)top
(global-set-key (kbd "C-c e <left>") 'emms-previous)
(global-set-key (kbd "C-c e <right>") 'emms-next)

(global-set-key (kbd "C-c e e") 'emms)

(provide 'microamp-emms)
