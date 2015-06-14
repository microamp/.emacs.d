(require 'go-mode)

;;(require 'flymake-go)
(require 'go-autocomplete)
(require 'go-errcheck)
(require 'helm-dash)

;; tab = 2 spaces
(setq go-tab-width 2)
;; goimports in favour of gofmt
(setq gofmt-command "goimports")
;; external dependencies
(setq external-packages '("github.com/tleyden/checkers-bot-minimax"
                          "github.com/kisielk/errcheck"
                          "github.com/nsf/gocode"
                          "code.google.com/p/rog-go/exp/cmd/godef"
                          "golang.org/x/tools/cmd/goimports"
                          "golang.org/x/tools/cmd/gorename"
                          "golang.org/x/tools/cmd/oracle"))
;; compile command
(setq command-list '("go build -v"
                     "go test -v"
                     "go vet"))
(setq go-compile-command (mapconcat 'identity command-list " && "))

;; install external packages via 'go get'
(dolist (p external-packages)
  (let ((command (concat "go get " p)))
    (message (concat "running '" command "'..."))
    (shell-command command)))

;; load oracle.el
(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

;; load rename.el
(load-file "$GOPATH/src/golang.org/x/tools/refactor/rename/rename.el")

;; set tab width
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width go-tab-width)))

;; goimports (see above) before saved
(add-hook 'before-save-hook 'gofmt-before-save)

;;(add-hook 'go-mode-hook 'flymake-go)

;; eldoc integration
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; helm-dash integration
(add-hook 'go-mode-hook
          (lambda ()
            (interactive)
            (setq-local helm-dash-docsets '("Go"))))

(defun go-keybinding-hooks ()
  ;; keybindings: compile
  (define-key go-mode-map (kbd "C-c C-c")
    (lambda ()
      (interactive)
      (compile go-compile-command)))
  ;; keybindings: imenu
  (define-key go-mode-map (kbd "C-c C-i") 'imenu)
  ;; keybindings: gofmt/godoc
  (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
  (define-key go-mode-map (kbd "C-c C-d") 'godoc)
  ;; keybindings: imports
  (define-key go-mode-map (kbd "C-c i i") 'go-import-add)
  (define-key go-mode-map (kbd "C-c i c") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-c i g") 'go-goto-imports)
  ;; keybindings: godef
  (define-key go-mode-map (kbd "C-c d") 'godef-describe)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (local-unset-key (kbd "C-*"))
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
  ;; keybindings: go playground (go hastebin)
  (define-key go-mode-map (kbd "C-c C-p b") 'go-play-buffer)
  (define-key go-mode-map (kbd "C-c C-p r") 'go-play-region)
  (define-key go-mode-map (kbd "C-c C-p d") 'go-download-play)
  ;; keybindings: errcheck
  (define-key go-mode-map (kbd "C-c C-e") 'go-errcheck)
  ;; keybindings: oracle
  (define-key go-mode-map (kbd "C-c o s") 'go-oracle-set-scope)
  (define-key go-mode-map (kbd "C-c o c") 'go-oracle-callers)
  ;; keybindings: rename
  (define-key go-mode-map (kbd "C-c C-r") 'go-rename)
  ;; keybindings: navigation (M-]/M-[ to jump to next/previous func)
  (define-key go-mode-map (kbd "M-[") 'beginning-of-defun)
  (define-key go-mode-map (kbd "M-]")
    (lambda ()
      (interactive)
      (end-of-defun)
      (end-of-defun)
      (beginning-of-defun))))

(add-hook 'go-mode-hook 'go-keybinding-hooks)

(provide 'microamp-go)
