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
(setq external-packages (make-hash-table :test 'equal))
(puthash :checkers-bot-minimax "github.com/tleyden/checkers-bot-minimax" external-packages)
(puthash :errcheck             "github.com/kisielk/errcheck"             external-packages)
(puthash :gocode               "github.com/nsf/gocode"                   external-packages)
(puthash :godef                "code.google.com/p/rog-go/exp/cmd/godef"  external-packages)
(puthash :goimports            "golang.org/x/tools/cmd/goimports"        external-packages)
(puthash :oracle               "golang.org/x/tools/cmd/oracle"           external-packages)
;; compile command
(setq command-list '("go build -v"
                     "go test -v"
                     "go vet"))
(setq go-compile-command (mapconcat 'identity command-list " && "))

;; install external packages via 'go get'
(maphash (lambda (k v)
           (let ((command (concat "go get " v)))
             (print (concat "running " command "..."))
             (shell-command command)))
         external-packages)

;; load oracle.el
(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

;; set tab width
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width go-tab-width)))

;; goimports (see above) before saved
(add-hook 'before-save-hook 'gofmt-before-save)

;;(add-hook 'go-mode-hook 'flymake-go)

;; helm-dash integration
(add-hook 'go-mode-hook (lambda ()
                          (interactive)
                          (setq-local helm-dash-docsets '("Go"))))

(defun go-keybinding-hooks ()
  ;; keybindings: compile
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (compile go-compile-command)))
  ;; keybindings: imenu
  (local-set-key (kbd "C-c C-i") 'imenu)
  ;; keybindings: gofmt/godoc
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-d") 'godoc)
  ;; keybindings: imports
  (local-set-key (kbd "C-c i i") 'go-import-add)
  (local-set-key (kbd "C-c i c") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i g") 'go-goto-imports)
  ;; keybindings: godef
  (local-set-key (kbd "C-c d") 'godef-describe)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-unset-key (kbd "C-*"))
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  ;; keybindings: go playground (go hastebin)
  (local-set-key (kbd "C-c C-p b") 'go-play-buffer)
  (local-set-key (kbd "C-c C-p r") 'go-play-region)
  (local-set-key (kbd "C-c C-p d") 'go-download-play)
  ;; keybindings: errcheck
  (local-set-key (kbd "C-c C-e") 'go-errcheck)
  ;; keybindings: oracle
  (local-set-key (kbd "C-c o s") 'go-oracle-set-scope)
  (local-set-key (kbd "C-c o c") 'go-oracle-callers)
  ;; keybindings: navigation (M-]/M-[ to jump to next/previous func)
  (local-set-key (kbd "M-[") 'beginning-of-defun)
  (local-set-key (kbd "M-]") (lambda ()
                               (interactive)
                               (end-of-defun)
                               (end-of-defun)
                               (beginning-of-defun))))

(add-hook 'go-mode-hook 'go-keybinding-hooks)

(provide 'microamp-go)
