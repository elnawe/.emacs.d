;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

(require 'mu4e)

(with-eval-after-load 'mu4e
  (require 'helm-mu)
  (require 'org-mu4e)
  (require 'mu4e-contrib)
  (require 'mu4e-context)

  (defun nemacs-mu4e ()
    "Runs the `mu4e' command and force update index."
    (interactive)
    (progn
      (mu4e)
      (mu4e-update-mail-and-index)))

  (defun nemacs-mu4e-update-and-find-unread-mail ()
    "Opens `mu4e-headers' with the parameter `flag:unread'. Used in Polybar."
    (interactive)
    (mu4e-update-index)
    (mu4e-headers-search "flag:unread"))

  (defun nemacs-email-get-mail ()
    "Runs the command to get the e-mail from the server."
    (interactive)
    (async-shell-command
     (expand-file-name "dotfiles/bin/pull_email" nemacs-dropbox-dir)
     "*pull_email*"))

  (defun nemacs-setup-mu4e-main-mode ()
    "NEMACS Setup: Run this function in `mu4e-main-mode-hook'."
    (local-set-key "s" #'helm-mu)
    (local-set-key "G" #'nemacs-email-get-mail))

  (defun nemacs-setup-mu4e-headers-mode ()
    "NEMACS Setup: Run this function in `mu4e-headers-mode-hook'."
    (local-set-key "s" #'helm-mu)
    (setq-local line-spacing 0.2))

  (defun nemacs-setup-mu4e-view-mode ()
    "NEMACS Setup: Run this function in `mu4e-view-mode-hook'."
    (local-set-key "s" #'helm-mu))

  (defun nemacs-setup-mu4e-compose-mode ()
    "NEMACS Setup: Run this function in `mu4e-compose-mode-hook'."
    (require 'boxquote)
    (local-set-key (kbd "C-`")  #'boxquote-text)
    (local-set-key (kbd "C-\"") #'boxquote-region)
    (local-set-key (kbd "C-~")  #'boxquote-title)
    (turn-on-visual-line-mode)
    (use-hard-newlines -1)
    (flyspell-mode))

  (add-hook 'mu4e-main-mode-hook    #'nemacs-setup-mu4e-main-mode)
  (add-hook 'mu4e-headers-mode-hook #'nemacs-setup-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook    #'nemacs-setup-mu4e-view-mode)
  (add-hook 'mu4e-compose-mode-hook #'nemacs-setup-mu4e-compose-mode)

  (global-set-key (kbd "C-x m")   #'nemacs-mu4e)
  (global-set-key (kbd "C-x M")   #'mu4e-compose-new)

  (setq message-cite-reply-position 'above
        message-cite-style message-cite-style-outlook
        message-kill-buffer-on-exit t
        mu4e-attachment-dir nemacs-downloads-dir
        mu4e-compose-context-policy 'pick-first
        mu4e-compose-dont-reply-to-self t
        mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-get-mail-command "true"
        mu4e-html2text-command "w3m -dump -T text/html -cols 120"
        mu4e-maildir nemacs-maildir-dir
        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval 300
        mu4e-user-mail-address-list '("nsacchetti@itx.com")
        mu4e-view-prefer-html t
        mu4e-view-show-images t
        org-mu4e-convert-to-html t)

  (setq mu4e-contexts `(,(make-mu4e-context
                     :name "ITX"
                     :vars '((user-mail-address             . "nsacchetti@itx.com")
                             (user-full-name                . "Nahuel Jesús Sacchetti")
                             (mu4e-drafts-folder            . "/itx/Drafts")
                             (mu4e-sent-folder              . "/itx/Sent Items")
                             (mu4e-refile-folder            . "/itx/Archive")
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
