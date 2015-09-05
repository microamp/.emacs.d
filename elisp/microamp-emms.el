(require 'emms-setup)
(emms-all)
(emms-default-players)

(setq emms-source-file-default-directory "~/Music/")

(global-set-key (kbd "C-c e <up>") 'emms-start)
(global-set-key (kbd "C-c e <down>") 'emms-stop)
(global-set-key (kbd "C-c e <left>") 'emms-previous)
(global-set-key (kbd "C-c e <right>") 'emms-next)

(global-set-key (kbd "C-c e e") 'emms)

(provide 'microamp-emms)
