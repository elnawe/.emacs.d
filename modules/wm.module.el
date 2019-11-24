(require 'exwm)

(with-eval-after-load 'exwm
  (require 'exwm-config)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (require 'nemacs-fn-keys)

  (defun nemacs-exwm-rename-buffer ()
    "Rename the buffers to the window title."
    (exwm-workspace-rename-buffer
     (concat (capitalize exwm-class-name) " - " exwm-title)))

  (defun nemacs-exwm-take-screenshot ()
    "Starts `scrot' to take a screenshot. The screenshot is saved
in `/tmp/' and also copied into the clipboard."
    (interactive)
    (start-process-shell-command
     "scrot"
     nil
     "scrot -s '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"))

  (defun nemacs-exwm-run-application (command)
    "Prompts for an application and runs it inside `EXWM'."
    (interactive (list (read-shell-command "> ")))
    (start-process-shell-command command nil command))

  (defun nemacs-exwm-exit-logout ()
    "Logout from session after closing NEMACS.

Overrides the `nemacs-prompt-before-exiting-emacs' (C-x C-c) and
runs `emacs-kill-hook' before closing the session."
    (interactive)
    (run-hook-with-args-until-success 'emacs-kill-hook)
    (nemacs-exwm-run-application "xfce4-session-logout"))

  (define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)
  
  (global-set-key (kbd "C-x C-c") #'save-buffers-kill-emacs)

  (add-hook 'exwm-update-title-hook #'nemacs-exwm-rename-buffer)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  (setq exwm-input-global-keys
        `(([?\s-&]                . nemacs-exwm-run-application)
          ([?\s-r]                . exwm-reset)
          ([print]                . nemacs-exwm-take-screenshot)
          ([?\s-S]                . nemacs-exwm-take-screenshot)
          
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

  (exwm-systemtray-enable)
  (exwm-enable))
