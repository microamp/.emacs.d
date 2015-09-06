(require 'ensime)
(require 'sbt-mode)
(require 'scala-mode2)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(defun scala-fmt ()
  (when (eq major-mode 'scala-mode)
    (ensime-format-source)))

(add-hook 'before-save-hook #'scala-fmt)

;; helm-dash integration
(add-hook 'scala-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Scala"))))

;; use tab in yasnippet, disable tab in company-mode
(add-hook 'scala-mode-hook #'yas-minor-mode)
(define-key company-active-map [tab] nil)

(define-key scala-mode-map (kbd "C-c C-e") 'ensime)
(define-key scala-mode-map (kbd "C-c C-b M-s") 'sbt-start)

(provide 'microamp-scala)
