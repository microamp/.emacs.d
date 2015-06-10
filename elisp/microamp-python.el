(require 'fill-column-indicator)
(eval-after-load "flymake" '(require 'flymake-cursor))
(eval-after-load "flymake" '(require 'flymake-python-pyflakes))
(require 'jedi)
(require 'helm-dash)
(require 'helm-pydoc)

;; ipython is the shell to go
(setq python-shell-interpreter "ipython")

;; vertical line to indicate column width limit (pep8 revised)
(setq-default fci-rule-column 99)
(setq-default fci-rule-color "#F0DFAF")

;; flake8 linting
(setq flymake-python-pyflakes-executable "flake8")

;; max line length now 99 (pep8 revised)
(setq flymake-python-pyflakes-extra-arguments '("--max-line-length=99"))

;; jedi (for auto-completion)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(jedi:install-server)

(setq jedi:install-imenu t)

;; jedi shortcuts
(setq jedi:use-shortcuts t) ; M-.: jedi:goto-definition, M-,: jedi:goto-definition-pop-marker

;; (jedi:tooltip-method '(pos-tip popup))
(setq jedi:tooltip-method nil)

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'run-python)))

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c f m") 'flymake-mode)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

;; python hooks
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda ()
                              (interactive)
                              (setq-local helm-dash-docsets '("Python 2"
                                                              "Python 3"))))

(define-key python-mode-map (kbd "<backtab>") 'god-mode-all)

(provide 'microamp-python)
