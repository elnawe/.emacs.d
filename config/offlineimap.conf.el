(use-package offlineimap
  :preface
  (defun nemacs-schedule-offlineimap-process ()
    "Runs the `offlineimap' process immediatly and then repeat every 15 minutes."
    (interactive)
    (run-with-timer 10 (* 15 60) 'offlineimap))
  :config
  (nemacs-schedule-offlineimap-process)
  :custom
  (offlineimap-command (concat user-emacs-directory "bin/poll_mail")))
