(require 'geiser-mode)

(setq geiser-active-implementations '(racket))

(add-hook 'scheme-mode-hook 'geiser-mode)

(provide 'microamp-racket)
