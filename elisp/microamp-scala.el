(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; helm-dash integration
(add-hook 'scala-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Scala"))))

(provide 'microamp-scala)
