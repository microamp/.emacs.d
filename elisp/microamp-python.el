(require 'fill-column-indicator)
(eval-after-load "flymake" '(require 'flymake-cursor))
(eval-after-load "flymake" '(require 'flymake-python-pyflakes))
;(require 'ipython)
(require 'jedi)
(require 'helm-pydoc)

;;; use IPython as default shell
;(setq-default py-shell-name "ipython")
;
;;; switch to interpreter after executing code
;(setq py-shell-switch-buffers-on-execute-p t)
;(setq py-switch-buffers-on-execute-p t)

;; run ipython inside shell
(setq python-shell-interpreter "ipython")
(define-key global-map (kbd "C-x I") 'python-shell-switch-to-shell)

;; fill column indicator
(setq-default fci-rule-column 79)
(setq-default fci-rule-color "#F0DFAF")

;; flake8 linting
(setq flymake-python-pyflakes-executable "flake8")

;; jedi (for auto-completion)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(jedi:install-server)

(setq jedi:install-imenu t)

;; jedi shortcuts
(setq jedi:use-shortcuts t) ; M-.: jedi:goto-definition, M-,: jedi:goto-definition-pop-marker

;; (jedi:tooltip-method '(pos-tip popup))
(setq jedi:tooltip-method nil)

;; jedi-direx (tree style source code viewer)
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'run-python)))

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c f m") 'flymake-mode)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

;; shortcut to switch to *Python* buffer
(define-key global-map (kbd "C-c C-p") (lambda ()
                                         (interactive)
                                         (switch-to-buffer "*Python*")))

;; python hooks
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'microamp-python)
