(require 'cider)

; enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook #'eldoc-mode)

; log communication with the nREPL server
(setq nrepl-log-messages t)

; hide the *nrepl-connection* and *nrepl-server* buffers from buffer list
(setq nrepl-hide-special-buffers t)

; tab to only indent in repl
(setq cider-repl-tab-command #'indent-for-tab-command)

; prefer local resources to remote (tramp) ones when both are available
(setq cider-prefer-local-resources t)

; prevent the auto-display of the REPL buffer in a separate window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

; disable auto display of error buffer with stacktraces when errored
(setq cider-show-error-buffer nil)

; disable auto-selection of the error buffer when it's displayed
(setq cider-auto-select-error-buffer nil)

; stacktrace filters (valid: java, clj, repl, tooling, and dup)
(setq cider-stacktrace-default-filters '(clj tooling dup))

; make stacktraces more readable by limiting column length to 80 chars
(setq cider-stacktrace-fill-column 80)

; buffer name to *cider-repl|project-name*
(setq nrepl-buffer-name-separator "* *")

; REPL buffer to show port no
(setq nrepl-buffer-name-show-port t)

; enable switching to REPL in current window (C-c C-z)
(setq cider-repl-display-in-current-window t)

; REPL prefix
(setq cider-repl-result-prefix "=> ")

; enable test report
(setq cider-test-show-report-on-success t)

; enable REPL history
(setq cider-repl-wrap-history t)

; set REPL history size (default: 500)
(setq cider-repl-history-size 1000)

; enable pretty-printing in REPL
(setq cider-repl-use-pretty-printing t)

; auto-completion (via company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)

; paredit
;(add-hook 'cider-mode-hook #'paredit-mode)
;(add-hook 'cider-repl-mode-hook #'paredit-mode)

; rainbow delimiters
;(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "C-c M-r") 'cider-namespace-refresh)

(provide 'microamp-clojure)
