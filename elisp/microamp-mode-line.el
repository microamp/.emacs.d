(require 'powerline)

(defun set-mode-line-format ()
  (interactive)
  (setq-default mode-line-format
                (list
                 " "
                 ;; the buffer name; the file name as a tool tip
                 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                     'help-echo (buffer-file-name)))
                 "| "

                 ;; line and column
                 "(" ;; '%02' to set to 2 chars at least; prevents flickering
                 (propertize "%02l" 'face 'font-lock-type-face) ","
                 (propertize "%02c" 'face 'font-lock-type-face)
                 ") "

                 ;; relative position, size of file
                 "["
                 (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
                 "/"
                 (propertize "%I" 'face 'font-lock-constant-face) ;; size
                 "] "

                 ;; the current major mode for the buffer.
                 "["
                 '(:eval (propertize "%m" 'face 'font-lock-string-face
                                     'help-echo buffer-file-coding-system))
                 "] "


                 "[" ;; insert vs overwrite mode, input-method in a tooltip
                 '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                     'face 'font-lock-preprocessor-face
                                     'help-echo (concat "Buffer is in "
                                                        (if overwrite-mode "overwrite" "insert") " mode")))

                 ;; was this buffer modified since the last save?
                 '(:eval (when (buffer-modified-p)
                           (concat "|"  (propertize "Mod"
                                                    'face 'font-lock-warning-face
                                                    'help-echo "Buffer has been modified"))))

                 ;; is this buffer read-only?
                 '(:eval (when buffer-read-only
                           (concat "|"  (propertize "RO"
                                                    'face 'font-lock-type-face
                                                    'help-echo "Buffer is read-only"))))
                 "] "

                 "{"
                 '(vc-mode vc-mode)
                 " } "

                 ;; i don't want to see minor-modes; but if you want, uncomment this:
                 ;minor-mode-alist

                 "%-" ;; fill with '-'
                 )))

(defun set-powerline-format ()
  (interactive)
  (powerline-default-theme))

(set-powerline-format)

(setq powerline-default-separator 'wave)

(provide 'microamp-mode-line)
