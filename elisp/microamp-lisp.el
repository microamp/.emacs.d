(require 'clojure-mode)
(require 'cider)
(require 'paredit)

;; clojure-specific (cider/nrepl)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-tab-command 'indent-for-tab-command)
(setq cider-prefer-local-resources t)
(setq cider-show-error-buffer nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-stacktrace-fill-column 80)
(setq nrepl-buffer-name-separator "-")
(setq cider-repl-result-prefix "=> ")

; C-c C-z to switch to cider repl in current window
(setq cider-repl-display-in-current-window t)
; repl not prompted to save after modified
(setq cider-prompt-save-file-on-load nil)

; repl history
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)  ; the default is 500

;; hooks for various lisp modes
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'hy-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(provide 'microamp-lisp)
