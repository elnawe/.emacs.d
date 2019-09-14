(use-package exwm
  :preface
  (defun nemacs-exwm-update-class ()
    "Rename buffer classes for X windows."
    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                (string= "gimp" exwm-instance-name))
      (exwm-workspace-rename-buffer exwm-class-name)))

  (defun nemacs-exwm-launch-command (command)
    "Launches a specific `command'. E.g. firefox."
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))
  :hook
  (exwm-update-class-hook . nemacs-exwm-update-class)
  :custom
  (exwm-workspace-number 1)
  (exwm-input-global-keys `(([?\s-&] . nemacs-exwm-launch-command)))
  (exwm-input-simulation-keys '(([?\C-b] . [left])
                                ([?\M-b] . [C-left])
                                ([?\C-f] . [right])
                                ([?\M-f] . [C-right])
                                ([?\C-p] . [up])
                                ([?\C-n] . [down])
                                ([?\C-a] . [home])
                                ([?\C-e] . [end])
                                ([?\M-v] . [prior])
                                ([?\C-v] . [next])
                                ([?\C-d] . [delete])
                                ([?\C-k] . [S-end delete])
                                ([?\C-w] . [?\C-x])
                                ([?\M-w] . [?\C-c])
                                ([?\C-y] . [?\C-v])
                                ([?\C-s] . [?\C-f])))
  :init
  (require 'exwm)
  (exwm-enable))
