(use-package boxquote
  :if (eq system-type 'gnu/linux))

(use-package message
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-boxquote-region-or-input ()
    (interactive)
    (if (use-region-p)
        (boxquote-region (region-beginning) (region-end))
      (call-interactively #'boxquote-text)))

  (defun nemacs-my-signature ()
    (concat
     "Nahuel Jesús Sacchetti\n"
     "Solution Lead for Monsters at ITX\n"
     "Learn more about ITX at https://www.itx.com/\n"
     "and about me at https://nsacchetti.com\n\n"

     "Message sent from GNU Emacs " emacs-version "."))
  :hook (message-mode . footnote-mode)
  :bind (:map message-mode-map
              ("C-c ^" . org-contacts)
              ("M-Q"   . nemacs-boxquote-region-or-input)
              ("C-M-y" . boxquote-yank)
              ("C-Q"   . boxquote-title))
  :custom
  (user-mail-address "me@nsacchetti.com")
  (user-full-name "Nahuel Jesús Sacchetti")
  (mail-from-style nil)
  (mail-user-agent 'gnus-user-agent)
  (mail-sources '((file :path "/var/mail/njs")))
  (message-auto-save-directory "~/Messages/Gnus/drafts")
  (message-confirm-send t)
  (message-default-mail-headers "Cc: \n")
  (message-directory "~/Messages/Gnus/")
  (message-dont-reply-to-names '("\\(me\\|nsacchetti\\)@\\(nsacchetti\\|itx\\|paychex\\)\\.com"))
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it)
  (message-signature #'nemacs-my-signature)
  (read-mail-command 'gnus))
