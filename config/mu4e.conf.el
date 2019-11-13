(use-package helm-mu
  :after (mu4e helm)
  :bind ((:map mu4e-main-mode-map
               ("s" . helm-mu))
         (:map mu4e-headers-mode-map
               ("s" . helm-mu))
         (:map mu4e-view-mode-map
               ("s" . helm-mu))))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :bind (("C-x m" . mu4e)
         ("C-x M" . mu4e-compose-new))
  :preface
  (require 'org-mu4e)
  (require 'mu4e-context)

  (defun nemacs-compose-setup-mode ()
    "Start this mode when `mu4e-compose-mode-hook' runs."
    (turn-on-visual-line-mode)
    (use-hard-newlines -1)
    (flyspell-mode))

  (defun nemacs-compose-turn-on-org ()
    "Turns on `org-mu4e-compose-org-mode' to send HTML e-mails."
    (interactive)
    (org-mu4e-compose-org-mode))

  (defun nemacs-get-email ()
    "Calls the `pull_email' script."
    (interactive)
    (async-shell-command "~/Dropbox/dotfiles/bin/pull_email" "*pull_email*"))
  :hook ((mu4e-view-mode    . turn-on-visual-line-mode)
         (mu4e-compose-mode . nemacs-compose-setup-mode))
  :bind ((:map mu4e-main-mode-map
               ("G" . nemacs-get-email)))
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-get-mail-command "true")
  (mu4e-update-interval 120)
  (mu4e-view-prefer-html t)
  (mu4e-view-show-images t)
  (mu4e-html2text-command "w3m -dump -T text/html")
  (mu4e-compose-format-flowed t)
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (org-mu4e-convert-to-html t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'pick-first)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-contexts `(,(make-mu4e-context
                     :name "ITX"
                     :vars '((user-mail-address             . "nsacchetti@itx.com")
                             (user-full-name                . "Nahuel Jesús Sacchetti")
                             (mu4e-sent-folder              . "/itx/Sent Items")
                             (mu4e-drafts-folder            . "/itx/Drafts")
                             (mu4e-trash-folder             . "/itx/Deleted Items")
                             (mu4e-compose-signature        . (concat "Nahuel Jesús Sacchetti\n"
                                                                      "ITX Solutions Architect\n\n"
                                                                      "Sent using Emacs " emacs-version))
                             (mu4e-compose-format-flowed    . t)
                             (smtpmail-queue-dir            . "~/Maildir/itx/queue/cur")
                             (message-send-mail-function    . smtpmail-send-it)
                             (smtpmail-smtp-user            . "nsacchetti@itx.com")
                             (smtpmail-starttls-credentials . (("smtp.office365.com" 587 nil nil)))
                             (smtpmail-auth-credentials     . (expand-file-name "~/.authinfo"))
                             (smtpmail-default-smtp-server  . "smtp.office365.com")
                             (smtpmail-smtp-server          . "smtp.office365.com")
                             (smtpmail-smtp-service         . 587)
                             (smtpmail-debug-info           . t)
                             (smtpmail-debug-verbose        . t)
                             (mu4e-maildir-shortcuts        . (("/itx/INBOX"            . ?i)
                                                               ("/itx/NEXT"             . ?n)
                                                               ("/itx/Sent Items"       . ?s)
                                                               ("/itx/Deleted Items"    . ?d))))))))
