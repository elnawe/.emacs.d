(require 'message)
(require 'smtpmail)

(global-set-key (kbd "C-x m") #'gnus)
(global-set-key (kbd "C-x M") #'compose-mail)

(with-eval-after-load 'message
  (defun nemacs-setup-message-mode ()
    "NEMACS Setup: Run this function in `message-mode-hook'."
    (require 'boxquote)
    (local-set-key (kbd "C-`")  #'boxquote-text)
    (local-set-key (kbd "C-\"") #'boxquote-region)
    (local-set-key (kbd "C-~")  #'boxquote-title)
    (turn-off-auto-fill)
    (turn-on-visual-line-mode)
    (use-hard-newlines -1)
    (flyspell-mode))

  (defun nemacs-signature ()
    "Signature to be attached to e-mails."
    (concat "Nahuel Jesús Sacchetti\n"
            "ITX Solutions Architect\n\n"
            "Sent using Emacs " emacs-version))

  (add-hook 'message-mode-hook #'nemacs-setup-message-mode)

  (setq message-cite-reply-position 'above
        message-cite-style message-cite-style-outlook
        message-kill-buffer-on-exit t
        message-send-mail-function #'smtpmail-send-it
        message-signature 'nemacs-signature))

(with-eval-after-load 'smtpmail
  (setq smtpmail-auth-credentials (expand-file-name "~/.authinfo")
        smtpmail-debug-info t
        smtpmail-debug-verbose t
        smtpmail-default-smtp-server "smtp.office365.com"
        smtpmail-queue-dir "~/Maildir/itx/queue/cur"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587
        smtpmail-smtp-user "nsacchetti@itx.com"
        smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
        user-full-name "Nahuel Jesús Sacchetti"
        user-mail-address "nsacchetti@itx.com"))
