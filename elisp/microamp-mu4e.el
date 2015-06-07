(require 'mu4e)

;; path to our Maildir directory
(setq mu4e-maildir "~/Maildir")

;; the next are relative to `mu4e-maildir'
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
(setq mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash")

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq mu4e-maildir-shortcuts
      '(("/archive"     . ?a)
        ("/inbox"       . ?i)
        ("/work"        . ?w)
        ("/sent"        . ?s)))

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("sangho.nah@gmail.com"))

;; when you want to use some external command for text->html
;; conversion, e.g. the 'html2text' program
;; (setq mu4e-html2text-command "html2text")

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
      '((:date          .  25)    ;; alternatively, use :human-date
        (:flags         .   6)
        (:from          .  22)
        (:subject       .  nil))) ;; alternatively, use :thread-subject

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
(setq mu4e-get-mail-command "offlineimap")

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "sangho.nah@gmail.com"
      user-mail-address     "sangho.nah@gmail.com"
      user-full-name        "james sangho nah")
;;(setq mu4e-compose-signature
;;   "blahblah\nhttp://www.example.com\n")

;; smtp mail setting
(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server         "smtp.gmail.com"
      smtpmail-local-domain        "gmail.com"

      ;; if you need offline mode, set these -- and create the queue dir
      ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
      smtpmail-queue-mail  nil
      smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'microamp-mu4e)
