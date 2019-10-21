(use-package smtpmail
  :ensure nil
  :preface
  (defun nemacs-my-signature ()
    (concat
     "Nahuel Jesus Sacchetti\n"
     "ITX | Solutions Architect\n"
     "Learn more about ITX at https://www.itx.com/\n"
     "and about me at https://nsacchetti.com\n\n"

     "Message sent from GNU Emacs " emacs-version "."))
  :custom
  (message-signature 'nemacs-my-signature)
  (gnutls-verify-error t)
  (smtpmail-queue-dir "~/Messages/queued-mail")
  (nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir))
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t)
  (starttls-use-gnutls t)
  (smtpmail-smtp-server "smtp.office365.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls))
