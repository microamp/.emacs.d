(require 'eredis)

(setq ehost-local "localhost"
      eport-local "6379")

(defun eredis-connect-local ()
  (interactive)
  (eredis-hai ehost-local eport-local) ;; connect to localhost:6379
  (message (format "connected to %s:%s (PING? %s)"
                   ehost-local
                   eport-local
                   (eredis-ping))))

(provide 'microamp-redis)
