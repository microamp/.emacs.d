(require 'geiser-mode)

(setq geiser-active-implementations '(racket))

(add-hook 'scheme-mode-hook 'geiser-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(provide 'microamp-racket)
