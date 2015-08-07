;; sample setup for postgres
(setq sql-connection-alist
      '((postgres-local (sql-product 'postgres)
                        (sql-user (getenv "USER"))
                        (sql-server "localhost")
                        (sql-port 5432)
                        (sql-database "postgres"))))

;; line truncation off
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(defun connect-postgres (conn-info)
  (setq sql-product 'postgres)
  (sql-connect conn-info))

;; shortcuts for sql interfaces
(define-key global-map (kbd "C-x M-s m") 'sql-mysql)
(define-key global-map (kbd "C-x M-s s") 'sql-sqlite)
(define-key global-map (kbd "C-x M-s p")
  (lambda ()
    (interactive)
    (connect-postgres 'postgres-local)))

(provide 'microamp-sql)
