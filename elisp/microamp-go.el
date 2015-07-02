;; tab = 2 spaces
(setq go-tab-width 2)
;; goimports in favour of gofmt
(setq gofmt-command "goimports")
;; external dependencies
(setq external-packages '("code.google.com/p/rog-go/exp/cmd/godef"
                          "github.com/golang/lint/golint"
                          "github.com/jstemmer/gotags"
                          "github.com/kisielk/errcheck"
                          "github.com/nsf/gocode"
                          "github.com/tleyden/checkers-bot-minimax"
                          "github.com/tools/godep"
                          "golang.org/x/tools/cmd/goimports"
                          "golang.org/x/tools/cmd/gorename"
                          "golang.org/x/tools/cmd/oracle"))
;; compile command
(setq command-list '("go build -v"
                     "go test -v"
                     "go vet"))
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

;; helm-dash integration
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local helm-dash-docsets '("Go"))))

;; define go-specific compile command
(add-hook 'go-mode
          (lambda ()
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     go-compile-command))))

;; keybindings: compile
(define-key go-mode-map (kbd "C-c C-c") 'compile)

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

;; keybindings: godef
(define-key go-mode-map (kbd "C-c d") 'godef-describe) ;; redundant because go-eldoc
(local-unset-key (kbd "C-j"))
(local-unset-key (kbd "C-*"))
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

;; keybindings: go playground (go pastebin)
(define-key go-mode-map (kbd "C-c C-p b") 'go-play-buffer)
(define-key go-mode-map (kbd "C-c C-p r") 'go-play-region)
(define-key go-mode-map (kbd "C-c C-p d") 'go-download-play)

;; keybindings: errcheck
(define-key go-mode-map (kbd "C-c C-e") 'go-errcheck)

;; keybindings: oracle
(define-key go-mode-map (kbd "C-c C-o s") 'go-oracle-set-scope)
(define-key go-mode-map (kbd "C-c C-o c") 'go-oracle-callers)

;; keybindings: rename
(define-key go-mode-map (kbd "C-c M-r") 'go-rename)

;; keybindings: golint
(define-key go-mode-map (kbd "C-c C-l") 'golint)

(provide 'microamp-go)
