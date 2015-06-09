(require 'flymake-jslint)
(require 'js2-mode)
(require 'nodejs-repl)

;; js2-mode instead of built-in js-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; two spaces for indentation (js-mode)
;(setq js-indent-level 2)

;; two spaces for indentation (js2-mode)
;(setq js2-basic-offset 2)

;; js2-mode hooks
(add-hook 'js2-mode-hook
          (lambda () (setq js2-basic-offset 2)))
(add-hook 'js2-mode-hook 'flymake-jslint-load)
(add-hook 'js2-mode-hook
          (lambda () (local-set-key (kbd "C-c f m") 'flymake-mode)))
(add-hook 'js2-mode-hook
          (lambda () (local-set-key (kbd "C-c C-j") 'nodejs-repl)))

;; helm-dash integration
(add-hook 'js2-mode-hook (lambda ()
                           (interactive)
                           (setq-local helm-dash-docsets '("AngularJS"))))

(provide 'microamp-js)
