(require 'fill-column-indicator)
(eval-after-load "flymake" '(require 'flymake-cursor))
(eval-after-load "flymake" '(require 'flymake-python-pyflakes))
(require 'jedi)
(require 'helm-dash)
(require 'helm-pydoc)

(setq
 ;; ipython to be default shell
 python-shell-interpreter "ipython"

 python-max-line-length 99 ;; instead of 79 (see PEP-8 for more info)

 ;; linting settings (flake8)
 flake8-extra-arguments (list (concat "--max-line-length=" (number-to-string python-max-line-length)))
 flymake-python-pyflakes-executable "flake8"
 flymake-python-pyflakes-extra-arguments flake8-extra-arguments

 ;; auto-formatter settings (autopep8)
 autopep8-extra-arguments (list (concat "--max-line-length=" (number-to-string python-max-line-length)))
 py-autopep8-options autopep8-extra-arguments

 ;; auto completion settings (jedi)
 jedi:setup-keys t
 jedi:complete-on-dot t
 jedi:install-imenu t
 jedi:use-shortcuts t
 jedi:tooltip-method nil

 ;; dash docsets
 python-docsets '("Python 2" "Python 3" "Redis"))

;; highlight TODO, FIXME, etc.
(add-hook 'python-mode-hook 'hl-todo-mode)

;; vertical line
(setq-default
 fci-rule-column python-max-line-length)

(jedi:install-server)

(define-key python-mode-map (kbd "C-c C-p") 'run-python)
(define-key python-mode-map (kbd "C-c f m") 'flymake-mode)
(define-key python-mode-map (kbd "<backtab>") 'god-mode-all)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

;; python hooks
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets python-docsets)))
(add-hook 'jedi-mode-hook
          (lambda () (remove-hook 'after-change-functions
                                  'jedi:after-change-handler t)))
(add-hook 'python-mode-hook 'highlight-indentation-mode)
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(provide 'microamp-python)
