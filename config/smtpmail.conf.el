(use-package smtpmail
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-my-signature ()
    (concat
     "Nahuel Jesús Sacchetti\n"
     "Solution Lead for Monsters at ITX\n"
     "Learn more about ITX at https://www.itx.com/\n"
     "and about me at https://nsacchetti.com\n\n"

     "Message sent from GNU Emacs " emacs-version "."))
  :custom
  (gnutls-verify-error t)
  (smtpmail-queue-dir "~/Messages/Gnus/queued-mail")
  (nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir))
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t)
  (starttls-use-gnutls t)
  (smtpmail-smtp-server "smtp.fastmail.com")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (tls-checktrust gnutls-verify-error)
  (tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                     ;; compatibility fallbacks
                     "gnutls-cli -p %p %h"
                     "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")))
