(require 'tuareg)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
              auto-mode-alist))

;; -- opam and utop setup --------------------------------
;; setup environment variables using opam
(defun opam-vars ()
  (let* ((x (shell-command-to-string "opam config env"))
         (x (split-string x "\n"))
         (x (remove-if (lambda (x) (equal x "")) x))
         (x (mapcar (lambda (x) (split-string x ";")) x))
         (x (mapcar (lambda (x) (car x)) x))
         (x (mapcar (lambda (x) (split-string x "=")) x)))
    x))

(dolist (var (opam-vars))
  (setenv (car var) (substring (cadr var) 1 -1)))

;; update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))

;; update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
              "/../../share/emacs/site-lisp") load-path)

;; automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

(provide 'microamp-ocaml)
