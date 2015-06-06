(require 'go-mode)
(require 'go-errcheck)
;;(require 'flymake-go)

(exec-path-from-shell-copy-env "GOPATH")

(setq go-tab-width 2) ;; tab = 2 spaces

(defun go-go-to-next-func ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))

(defun go-compile ()
  (interactive)
  (compile (concat "go build" " " (buffer-file-name))))

;; set tab width (2 spaces)
(add-hook 'go-mode-hook
  (function
   (lambda ()
     (setq tab-width go-tab-width))))

;; gofmt before saved
(add-hook 'before-save-hook 'gofmt-before-save)

;;(add-hook 'go-mode-hook 'flymake-go)

(defun go-keybinding-hooks ()
  ;; compile
  (local-set-key (kbd "C-c C-c") 'go-compile)
  ;; gofmt and godoc
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-d") 'godoc)
  ;; import
  (local-set-key (kbd "C-c C-i C-i") 'go-import-add)
  (local-set-key (kbd "C-c C-i C-c") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-i C-g") 'go-goto-imports)
  ;; godef
  (local-set-key (kbd "C-c d") 'godef-describe)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-unset-key (kbd "C-*"))
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  ;; go playground
  (local-set-key (kbd "C-c C-p b") 'go-play-buffer)
  (local-set-key (kbd "C-c C-p r") 'go-play-region)
  (local-set-key (kbd "C-c C-p d") 'go-download-play)
  ;; go errcheck
  (local-set-key (kbd "C-c C-e") 'go-errcheck)
  ;; navigation (in addition to smartparens)
  (local-set-key (kbd "M-[") 'beginning-of-defun)
  (local-set-key (kbd "M-]") 'go-go-to-next-func))

(add-hook 'go-mode-hook 'go-keybinding-hooks)

(provide 'microamp-go)
