(require 'fill-column-indicator)
(eval-after-load "flymake" '(require 'flymake-cursor))
(eval-after-load "flymake" '(require 'flymake-python-pyflakes))
(require 'jedi)
(require 'helm-dash)
(require 'helm-pydoc)

(setq
 ;; ipython to be default shell
 python-shell-interpreter "ipython"
 ;; linting settings
 python-max-line-length 99
 flake8-extra-arguments (list (concat "--max-line-length=" (number-to-string python-max-line-length)))
 flymake-python-pyflakes-executable "flake8"
 flymake-python-pyflakes-extra-arguments flake8-extra-arguments
 ;; jedi settings
 jedi:setup-keys t
 jedi:complete-on-dot t
 jedi:install-imenu t
 jedi:use-shortcuts t
 jedi:tooltip-method nil
 ;; dash docsets
 python-docsets '("Python 2" "Python 3"))

;; vertical line
(setq-default
 fci-rule-column python-max-line-length
 fci-rule-color "#F0DFAF")

(jedi:install-server)

(define-key python-mode-map (kbd "C-c C-p") 'run-python)
(define-key python-mode-map (kbd "C-c f m") 'flymake-mode)
(define-key python-mode-map (kbd "<backtab>") 'god-mode-all)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

;; python hooks
(add-hook 'python-mode-hook 'fci-mode)
;;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets python-docsets)))

(provide 'microamp-python)
