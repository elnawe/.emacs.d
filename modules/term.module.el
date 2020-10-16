(when nemacs-module-term-enabled
  (require 'vterm)

  (with-eval-after-load 'vterm
    (require 'nemacs-ensure-system)

    (nemacs-ensure-system-package "cmake" t)

    (defun nemacs-create-switch-to-vterm ()
      "Switch to `vterm' buffer if exists. If not, automatically creates one."
      (interactive)
      (if (buffer-live-p (get-buffer "vterm"))
          (switch-to-buffer "vterm")
        (vterm)))

    (defun nemacs-create-switch-to-vterm-other-window ()
      "Split window and switch to `vterm' buffer if exists. If not, automatically creates one."
      (interactive)
      (if (buffer-live-p (get-buffer "vterm"))
          (progn (nemacs-create-window-right-and-switch)
                 (switch-to-buffer "vterm"))
        (vterm-other-window)))

    (global-set-key (kbd "C-x t") #'nemacs-create-switch-to-vterm)
    (global-set-key (kbd "C-x T") #'nemacs-create-switch-to-vterm-other-window)))
