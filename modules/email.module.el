;; TODO: Change to mbsync with notmuch

(require 'message)
(require 'notmuch)
(require 'smtpmail)

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

  (global-set-key (kbd "C-x M") #'notmuch-mua-new-mail)

  (setq message-cite-reply-position 'above
        message-cite-style message-cite-style-outlook
        message-kill-buffer-on-exit t
        message-send-mail-function #'smtpmail-send-it
        message-signature 'nemacs-signature
        mm-text-html-renderer 'w3m-standalone))

(with-eval-after-load 'notmuch
  (require 'nemacs-notmuch-tags)
  (require 'nemacs-notmuch-unread)
  (require 'org-notmuch)

  (defun nemacs-setup-notmuch-search-mode ()
    "NEMACS Setup: Run thi sin `notmuch-search-hook'."
    (beacon-mode -1)
    (hl-line-mode)
    (setq-local line-spacing 0.2))

  (defun nemacs-setup-notmuch-show-mode ()
    "NEMACS Setup: Run this in `notmuch-show-hook'."
    (variable-pitch-mode 0))

  (add-hook 'notmuch-search-hook #'nemacs-setup-notmuch-search-mode)
  (add-hook 'notmuch-show-hook #'nemacs-setup-notmuch-show-mode)

  (define-key notmuch-search-mode-map (kbd "C-?") #'nemacs-notmuch-toggle-tag-interactively)
  (define-key notmuch-search-mode-map "D" #'nemacs-notmuch-toggle-deleted)
  (define-key notmuch-search-mode-map "i" #'nemacs-notmuch-toggle-inbox)
  (define-key notmuch-search-mode-map "S" #'nemacs-notmuch-toggle-spam)

  (define-key notmuch-show-mode-map (kbd "C-?") #'nemacs-notmuch-toggle-tag-interactively)
  (define-key notmuch-show-mode-map "D" #'nemacs-notmuch-toggle-deleted)
  (define-key notmuch-show-mode-map "i" #'nemacs-notmuch-toggle-inbox)
  (define-key notmuch-show-mode-map "S" #'nemacs-notmuch-toggle-spam)

  (global-set-key (kbd "C-x m") #'notmuch)

  (setq notmuch-multipart/alternative-discouraged '("text/plain")
        notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "NEXT" :query "tag:NEXT" :key "n")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a"))
        notmuch-search-oldest-first nil
        notmuch-search-result-format `(("date" . "%12s ")
                                       ("count" . "%-6s ")
                                       ("authors" . "%-30s ")
                                       ("subject" . "%s ")
                                       ("tags" . "(%s)"))
        notmuch-show-relative-dates nil)

  (nemacs-notmuch-unread-mode))

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
