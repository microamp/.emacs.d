(require 'elixir-mode)
(require 'alchemist)

;; alchemist settings
(setq alchemist-help-ansi-color-docs nil) ;; default: t
(setq alchemist-project-compile-when-needed t) ;; default nil

;; hooks
(add-hook 'elixir-mode-hook 'alchemist-mode)

(provide 'microamp-elixir)
