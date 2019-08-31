(use-package notmuch
  :preface
  ;; All this code was taken from notmuch-unread
  ;; https://github.com/emacsattic/notmuch-unread
  (defvar notmuch-unread-mode-line-string nil
    "String to display in the mode line.")

  (defvar notmuch-unread-update-timer nil
    "Timer for updating the mode line.")

  (defcustom notmuch-unread-update-interval 5
    "The number of seconds to wait in between updates."
    :type 'integer
    :group 'notmuch-unread)

  (defcustom notmuch-unread-search-term "tag:unread"
    "The search term to pass to notmuch count."
    :type 'string
    :group 'notmuch-unread)

  (defun notmuch-unread-count ()
    "Return the number of messages that match
`notmuch-unread-search-term`."
    (string-to-number
     (replace-regexp-in-string
      "\n" ""
      (notmuch-command-to-string "count" notmuch-unread-search-term))))

  (defun notmuch-unread-update-handler ()
    "Update the mode line."
    (setq notmuch-unread-mode-line-string
          (format " [✉ %d]" (notmuch-unread-count)))
    (force-mode-line-update))

  (define-minor-mode notmuch-unread-mode
    "Display unread mail count in the mode line"
    :global t
    :require 'notmuch
    (and notmuch-unread-update-timer
         (cancel-timer notmuch-unread-update-timer))
    (if notmuch-unread-mode
        (progn
          (add-to-list 'global-mode-string
                       'notmuch-unread-mode-line-string t)
          (setq notmuch-unread-update-timer
                (run-at-time nil notmuch-unread-update-interval
                             'notmuch-unread-update-handler)))
      (setq global-mode-string
            (delq 'notmuch-unread-mode-line-string
                  global-mode-string))))
  :bind
  (("C-x m" . notmuch))
  :config
  (notmuch-unread-mode t)
  :custom
  (notmuch-multipart/alternative-discouraged '("text/plain"))
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format `(("date" . "%12s ")
                                  ("count" . "%-5s ")
                                  ("authors" . "%-20s ")
                                  ("subject" . "%s ")
                                  ("tags" . "(%s)"))))
