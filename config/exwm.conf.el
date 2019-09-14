(use-package exwm
  :preface
  (defun exwm-rename-buffer ()
    "Renames the `EXWM' buffers with the application name."
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))

  (defun nemacs-exwm-update-class ()
    "Rename buffer classes for X windows."
    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                (string= "gimp" exwm-instance-name))
      (exwm-workspace-rename-buffer exwm-class-name)))

  (defun nemacs-exwm-launch-command (command)
    "Launches a specific `command'. E.g. firefox."
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))
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

  ;; Hooks to rename application buffers.
  ;; *Note:* Needed here to force adding them before enabling `EXWM'.
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

  ;; Enable `EXWM'
  (exwm-enable))
