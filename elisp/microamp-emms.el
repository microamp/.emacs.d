(require 'emms-mode-line-cycle)
(require 'emms-mode-line-icon)
(require 'emms-setup)

(emms-all)
(emms-default-players)

(emms-mode-line 1)
(emms-playing-time 1)
(emms-mode-line-cycle 1)

(custom-set-variables
 '(emms-source-file-default-directory "~/Music/")
 '(emms-mode-line-cycle-max-width 15)
 '(emms-mode-line-cycle-additional-space-num 4)
 '(emms-mode-line-cycle-use-icon-p t)
 '(emms-mode-line-format " [%s]")
 '(emms-mode-line-cycle-current-title-function
   (lambda ()
     (let ((track (emms-playlist-current-selected-track)))
       (cl-case (emms-track-type track)
         ((streamlist)
          (let ((stream-name (emms-stream-name
                              (emms-track-get track 'metadata))))
            (if stream-name stream-name (emms-track-description track))))
         ((url) (emms-track-description track))
         (t (file-name-nondirectory
             (emms-track-description track))))))))

(global-set-key (kbd "C-c e p") 'emms-start) ;; (p)lay
(global-set-key (kbd "C-c e s") 'emms-stop) ;; (s)top
(global-set-key (kbd "C-c e <left>") 'emms-previous)
(global-set-key (kbd "C-c e <right>") 'emms-next)

(global-set-key (kbd "C-c e e") 'emms)

(provide 'microamp-emms)
