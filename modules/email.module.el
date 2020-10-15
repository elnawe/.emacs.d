;; (require 'mu4e)
;; (require 'message)
;; (require 'smtpmail)

;; (with-eval-after-load 'mu4e
;;   (require 'helm-mu)
;;   (require 'org-mu4e)
;;   (require 'mu4e-alert)
;;   (require 'mu4e-contrib)
;;   (require 'mu4e-context)
;;   (require 'mu4e-icalendar)

;;   (defun nemacs-mu4e ()
;;     "Runs the `mu4e' command and force update index."
;;     (interactive)
;;     (progn
;;       (mu4e)
;;       (mu4e-update-mail-and-index)))

;;   (defun nemacs-mu4e-update-mode-line ()
;;     (mu4e~proc-kill)
;;     (mu4e-alert-enable-mode-line-display))

;;   (defun nemacs-setup-mu4e-main-mode ()
;;     "NEMACS Setup: Run this function in `mu4e-main-mode-hook'."
;;     (local-set-key "s" #'helm-mu))

;;   (defun nemacs-setup-mu4e-headers-mode ()
;;     "NEMACS Setup: Run this function in `mu4e-headers-mode-hook'."
;;     (local-set-key "s" #'helm-mu)
;;     (setq-local line-spacing 0.2))

;;   (defun nemacs-setup-mu4e-view-mode ()
;;     "NEMACS Setup: Run this function in `mu4e-view-mode-hook'."
;;     (local-set-key "s" #'helm-mu))

;;   (defun nemacs-setup-mu4e-compose-mode ()
;;     "NEMACS Setup: Run this function in `mu4e-compose-mode-hook'."
;;     (require 'boxquote)
;;     (local-set-key (kbd "C-`")  #'boxquote-text)
;;     (local-set-key (kbd "C-\"") #'boxquote-region)
;;     (local-set-key (kbd "C-~")  #'boxquote-title)
;;     (turn-on-visual-line-mode)
;;     (use-hard-newlines -1)
;;     (flyspell-mode))

;;   (add-hook 'mu4e-main-mode-hook    #'nemacs-setup-mu4e-main-mode)
;;   (add-hook 'mu4e-headers-mode-hook #'nemacs-setup-mu4e-headers-mode)
;;   (add-hook 'mu4e-view-mode-hook    #'nemacs-setup-mu4e-view-mode)
;;   (add-hook 'mu4e-compose-mode-hook #'nemacs-setup-mu4e-compose-mode)

;;   (global-set-key (kbd "C-x m")   #'nemacs-mu4e)
;;   (global-set-key (kbd "C-x M")   #'mu4e-compose-new)

;;   (mu4e-icalendar-setup)

;;   (setq message-cite-reply-position 'above
;;         message-cite-style message-cite-style-outlook
;;         message-kill-buffer-on-exit t
;;         mu4e-attachment-dir nemacs-downloads-dir
;;         mu4e-compose-context-policy 'pick-first
;;         mu4e-compose-dont-reply-to-self t
;;         mu4e-confirm-quit nil
;;         mu4e-context-policy 'pick-first
;;         mu4e-get-mail-command "true"
;;         mu4e-maildir nemacs-maildir-dir
;;         mu4e-sent-messages-behavior 'delete
;;         mu4e-update-interval 300
;;         mu4e-user-mail-address-list '("nsacchetti@itx.com")
;;         mu4e-view-prefer-html t
;;         mu4e-view-show-images t
;;         mu4e-view-use-gnus t
;;         org-mu4e-convert-to-html t)

;;   (setq mm-text-html-renderer 'w3m
;;         mm-w3m-safe-url-regexp nil
;;         shr-use-colors nil)

;;   (setq mu4e-change-filenames-when-moving t)

;;   (setq gnus-icalendar-org-capture-file "~/Dropbox/Notes/calendar.org")
;;   (setq gnus-icalendar-org-capture-headline '("ITX")
;;         mu4e-icalendar-trash-after-reply t)
;;   (gnus-icalendar-org-setup)

;;   (setq mu4e-alert-interesting-mail-query "flag:unread maildir:/INBOX ")
;;   (mu4e-alert-enable-mode-line-display)

;;   (run-with-timer 0 60 #'nemacs-mu4e-update-mode-line)

;;   (setq mu4e-contexts `(,(make-mu4e-context
;;                      :name "ITX"
;;                      :vars '((user-mail-address             . "nsacchetti@itx.com")
;;                              (user-full-name                . "Nahuel Jesús Sacchetti")
;;                              (mu4e-drafts-folder            . "/Drafts")
;;                              (mu4e-sent-folder              . "/Sent")
;;                              (mu4e-refile-folder            . "/Archive")
;;                              (mu4e-trash-folder             . "/Deleted")
;;                              (mu4e-compose-signature        . (concat "Nahuel Jesús Sacchetti\n"
;;                                                                       "ITX Solutions Architect\n\n"
;;                                                                       "Sent using Emacs " emacs-version))
;;                              (mu4e-compose-format-flowed    . t)
;;                              (smtpmail-queue-dir            . "~/Maildir/queue/cur")
;;                              (message-send-mail-function    . smtpmail-send-it)
;;                              (smtpmail-smtp-user            . "nsacchetti@itx.com")
;;                              (smtpmail-starttls-credentials . (("smtp.office365.com" 587 nil nil)))
;;                              (smtpmail-auth-credentials     . (expand-file-name "~/.authinfo"))
;;                              (smtpmail-default-smtp-server  . "smtp.office365.com")
;;                              (smtpmail-smtp-server          . "smtp.office365.com")
;;                              (smtpmail-smtp-service         . 587)
;;                              (smtpmail-debug-info           . t)
;;                              (smtpmail-debug-verbose        . t)
;;                              (mu4e-maildir-shortcuts        . (("/INBOX"   . ?i)
;;                                                                ("/Archive" . ?a)
;;                                                                ("/Sent"    . ?s)
;;                                                                ("/Deleted" . ?d))))))))

;; (with-eval-after-load 'message
;;   (defun nemacs-setup-message-mode ()
;;     "NEMACS Setup: Run this function in `message-mode-hook'."
;;     (require 'boxquote)
;;     (local-set-key (kbd "C-`")  #'boxquote-text)
;;     (local-set-key (kbd "C-\"") #'boxquote-region)
;;     (local-set-key (kbd "C-~")  #'boxquote-title)
;;     (turn-off-auto-fill)
;;     (turn-on-visual-line-mode)
;;     (use-hard-newlines -1)
;;     (flyspell-mode))

;;   (defun nemacs-signature ()
;;     "Signature to be attached to e-mails."
;;     (concat "Nahuel Jesús Sacchetti\n"
;;             "ITX Solutions Architect\n\n"
;;             "Sent using Emacs " emacs-version))

;;   (add-hook 'message-mode-hook #'nemacs-setup-message-mode)

;;   (setq message-cite-reply-position 'above
;;         message-cite-style message-cite-style-outlook
;;         message-kill-buffer-on-exit t
;;         message-send-mail-function #'smtpmail-send-it
;;         message-signature 'nemacs-signature))

;; (with-eval-after-load 'smtpmail
;;   (setq smtpmail-auth-credentials (expand-file-name "~/.authinfo")
;;         smtpmail-debug-info t
;;         smtpmail-debug-verbose t
;;         smtpmail-default-smtp-server "smtp.office365.com"
;;         smtpmail-queue-dir "~/Maildir/queue/cur"
;;         smtpmail-smtp-server "smtp.office365.com"
;;         smtpmail-smtp-service 587
;;         smtpmail-smtp-user "nsacchetti@itx.com"
;;         smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
;;         user-full-name "Nahuel Jesús Sacchetti"
;;         user-mail-address "nsacchetti@itx.com"))
