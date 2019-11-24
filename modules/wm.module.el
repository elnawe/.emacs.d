(require 'exwm)

(with-eval-after-load 'exwm
  (require 'exwm-randr)
  (require 'nemacs-fn-keys)

  (defun nemacs-exwm-rename-buffer ()
    "Rename the buffers to the window title."
    (exwm-workspace-rename-buffer
     (concat (capitalize exwm-class-name) " - " exwm-title)))

  (defun nemacs-exwm-switch-to-laptop-screen ()
    "Switches to workspace in the Laptop screen."
    (interactive)
    (exwm-workspace-switch-create 0))

  (defun nemacs-exwm-move-window-to-laptop-screen ()
    "Moves current buffer to the Laptop screen."
    (interactive)
    (progn
      (exwm-workspace-move-window 0 (exwm--buffer->id (window-buffer)))
      (exwm-workspace-switch-create 0)))

  (defun nemacs-exwm-switch-to-external-screen ()
    "Switches to workspace in the external connected screen."
    (interactive)
    (exwm-workspace-switch-create 1))

  (defun nemacs-exwm-move-window-to-external-screen ()
    "Moves current buffer to the external connected screen."
    (interactive)
    (progn
      (exwm-workspace-move-window 1 (exwm--buffer->id (window-buffer)))
      (exwm-workspace-switch-create 1)))

  (defun nemacs-exwm-xrandr-check-update-monitor ()
    "Run this function when disconnecting a monitor. This will move everything
from the workspace of that monitor into the laptop screen."
    (let ((monitors (string-to-number (shell-command-to-string "xrandr --listactivemonitors | wc -l"))))
      (when (eq monitors 2)
        (message "Plugged external monitor is now disconnected.")
        (message "Moving windows to laptop monitor.")
        (exwm-workspace-delete 1)
        (nemacs-exwm-switch-to-laptop-screen))))

  (defun nemacs-exwm-take-screenshot ()
    "Starts `scrot' to take a screenshot. The screenshot is saved in `/tmp/' and
also copied into the clipboard."
    (interactive)
    (start-process-shell-command
     "scrot"
     nil
     "scrot -s '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"))

  (defun nemacs-exwm-run-application (command)
    "Prompts for an application and runs it inside `EXWM'."
    (interactive (list (read-shell-command "> ")))
    (start-process-shell-command command nil command))

  (defun nemacs-exwm-launch-wm ()
    "Runs this function when `EXWM' finishes loading. Meant to run the software
that complete my configuration."
    (async-shell-command "~/Dropbox/dotfiles/polybar/launch-polybar"))

  (defun nemacs-exwm-exit-logout ()
    "Logout from session after closing NEMACS.

Overrides the `nemacs-prompt-before-exiting-emacs' (C-x C-c) and runs
`emacs-kill-hook' before closing the session."
    (interactive)
    (run-hook-with-args-until-success 'emacs-kill-hook)
    (nemacs-exwm-run-application "xfce4-session-logout"))

  (add-hook 'exwm-update-title-hook #'nemacs-exwm-rename-buffer)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-randr-refresh-hook #'nemacs-exwm-xrandr-check-update-monitor)
  ;;(add-hook 'exwm-init-hook #'nemacs-exwm-launch-wm)

  (define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1")
        exwm-workspace-number 1)
  
  (setq exwm-input-global-keys
        `(([?\s-&]                . nemacs-exwm-run-application)
          ([?\s-r]                . exwm-reset)
          ([print]                . nemacs-exwm-take-screenshot)
          ([?\s-S]                . nemacs-exwm-take-screenshot)

          ([?\s-1]                . nemacs-exwm-switch-to-laptop-screen)
          ([?\s-!]                . nemacs-exwm-move-window-to-laptop-screen)
          ([?\s-2]                . nemacs-exwm-switch-to-external-screen)
          ([?\s-@]                . nemacs-exwm-move-window-to-external-screen)
          
          ([?\s-h]                . windmove-left)
          ([s-left]               . windmove-left)
          ([?\s-j]                . windmove-down)
          ([s-down]               . windmove-down)
          ([?\s-k]                . windmove-up)
          ([s-up]                 . windmove-up)
          ([?\s-l]                . windmove-right)
          ([s-right]              . windmove-right)

          ([?\s-H]                . buf-move-left)
          ([S-s-left]             . buf-move-left)
          ([?\s-J]                . buf-move-down)
          ([S-s-down]             . buf-move-down)
          ([?\s-K]                . buf-move-up)
          ([S-s-up]               . buf-move-up)
          ([?\s-L]                . buf-move-right)
          ([S-s-right]            . buf-move-right)
          
          ([?\s-c]                . helm-resume)
          ([?\s-b]                . helm-mini)
          
          ([?\s-Q]                . nemacs-kill-current-buffer)
          
          ([XF86AudioMute]        . nemacs-fn-key-mute-volume)
          ([XF86AudioLowerVolume] . nemacs-fn-key-decrease-volume)
          ([XF86AudioRaiseVolume] . nemacs-fn-key-increase-volume)
          ([XF86AudioMicMute]     . nemacs-fn-key-mute-microphone)))

  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\M-<] . [home])
          ([?\M->] . [end])
          ([?\C-s] . [?\C-f])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])))

  (exwm-randr-enable)
  (exwm-enable))
