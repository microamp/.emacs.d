;; tab = 4 spaces
(setq go-tab-width 4)
;; goimports in favour of gofmt
(setq gofmt-command "goimports")
;; external dependencies
(setq external-packages '("github.com/golang/lint/golint"
                          "github.com/jstemmer/gotags"
                          "github.com/kisielk/errcheck"
                          "github.com/nsf/gocode"
                          "github.com/rogpeppe/godef"
                          "github.com/tleyden/checkers-bot-minimax"
                          "github.com/tools/godep"
                          "golang.org/x/tools/cmd/goimports"
                          "golang.org/x/tools/cmd/gorename"
                          "golang.org/x/tools/cmd/oracle"))
;; compile command
(setq command-list '("go build -v"
                     "go test -v ./..."
                     "go vet ./..."))
(setq go-compile-command (mapconcat 'identity command-list " && "))
;; gopath from env
(setq gopath (getenv "GOPATH"))

;; install external packages via 'go get'
(dolist (p external-packages)
  (let ((command (concat "go get " p)))
    (message (concat "running '" command "'..."))
    (shell-command command)))

;; run `go run` shell command
(defun cmd-go-run ()
  (interactive)
  (shell-command (concat "go run " (buffer-file-name))))

;; load custom .el files
(dolist (el (list (concat gopath "/src/github.com/golang/lint/misc/emacs/golint.el")
                  (concat gopath "/src/golang.org/x/tools/cmd/oracle/oracle.el")
                  (concat gopath "/src/golang.org/x/tools/refactor/rename/rename.el")))
  (load-file el))

;; go-direx settings (to appear on the right)
(push '("^\*go-direx:"
        :regexp t
        :position right
        :width 0.4
        :dedicated t
        :stick t)
      popwin:special-display-config)

;; set tab width
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width go-tab-width)))

;; goimports (see above) before saved
(add-hook 'before-save-hook 'gofmt-before-save)

;; on-the-fly syntax checking
;(add-hook 'go-mode-hook 'flymake-go)

;; eldoc integration
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; highlight TODO, FIXME, etc.
(add-hook 'go-mode-hook 'hl-todo-mode)

;; helm-dash integration
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Go"))))

;; oracle scope updated as you switch to a different project
(defun get-go-project-root ()
  (interactive)
  (let ((gopath-src (concat gopath "/src/"))
        (project-root (string-remove-suffix "/" (projectile-project-root))))
    (string-remove-prefix gopath-src project-root)))

(defun set-oracle-scope ()
  (interactive)
  (let ((go-project-root (get-go-project-root)))
    (setq go-oracle-scope go-project-root)
    (message (concat "oracle scope: " go-oracle-scope))))

(advice-add 'helm-projectile-switch-project :after
            'set-oracle-scope)
(advice-add 'projectile-switch-project :after
            'set-oracle-scope)

;; keybindings: compile (NOTE: recommend to use 'projectile-compile-project instead)
(define-key go-mode-map (kbd "C-c C-c")
  (lambda ()
    (interactive)
    ;; define go-specific compile command (go build/test/vet)
    (set (make-local-variable 'compile-command)
         go-compile-command)
    (compile compile-command)))

;; keybindings: go run
(define-key go-mode-map (kbd "C-c C-r") 'cmd-go-run)

;; keybindings: go-direx (replacing imenu)
(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)

;; keybindings: gofmt/godoc
(define-key go-mode-map (kbd "C-c C-f") 'gofmt)
(define-key go-mode-map (kbd "C-c C-d") 'godoc)

;; keybindings: imports
(define-key go-mode-map (kbd "C-c C-i i") 'go-import-add)
(define-key go-mode-map (kbd "C-c C-i c") 'go-remove-unused-imports)
(define-key go-mode-map (kbd "C-c C-i g") 'go-goto-imports)

;; keybindings: godef (M-.: godef-jump and M-,: pop-tag-mark)
(define-key go-mode-map (kbd "C-c d") 'godef-describe) ;; redundant because go-eldoc
(define-key go-mode-map (kbd "C-j") nil)
(define-key go-mode-map (kbd "C-*") nil)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

;; keybindings: go playground (go pastebin)
(define-key go-mode-map (kbd "C-c C-p b") 'go-play-buffer)
(define-key go-mode-map (kbd "C-c C-p r") 'go-play-region)
(define-key go-mode-map (kbd "C-c C-p d") 'go-download-play)

;; keybindings: errcheck
;;(define-key go-mode-map (kbd "C-c C-e") 'go-errcheck-pkg)
(define-key go-mode-map (kbd "C-c C-e")
  (lambda ()
    (interactive)
    (go-errcheck-pkg (concat (get-go-project-root) "/...") nil nil nil)))

;; keybindings: oracle
(define-key go-mode-map (kbd "C-c C-o s") 'go-oracle-set-scope)
(define-key go-mode-map (kbd "C-c C-o c") 'go-oracle-callers)

;; keybindings: rename
(define-key go-mode-map (kbd "C-c M-r") 'go-rename)

;; keybindings: golint
(define-key go-mode-map (kbd "C-c C-l") 'golint)

(provide 'microamp-go)
