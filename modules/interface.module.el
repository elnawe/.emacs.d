(require 'all-the-icons)
(require 'beacon)
(require 'dashboard)
(require 'doom-modeline)

(with-eval-after-load 'all-the-icons
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (exwm-mode all-the-icons-faicon "laptop"))))

(with-eval-after-load 'beacon
  (beacon-mode))

(with-eval-after-load 'dashboard
  (defun nemacs-dashboard-banner ()
    (setq dashboard-banner-logo-title
          (format
           "NEMACS ready in %.2f seconds with %d garbage collections.\n"
           (float-time
            (time-subtract after-init-time before-init-time)) gcs-done)))

  (add-hook 'after-init-hook #'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook #'nemacs-dashboard-banner)

  (setq dashboard-startup-banner 'logo)

  (dashboard-setup-startup-hook))

(with-eval-after-load 'doom-modeline
  ;; NOTE: Need to set these variables before starting the modes.
  (setq battery-update-interval 15
        column-number-mode t
        display-time-24hr-format t
        display-time-default-load-average nil
        display-time-format "%b %d %H:%M "
        fancy-battery-show-percentage t
        line-number-mode t)

  (setq doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-buffer-encoding nil
        doom-modeline-height 25
        doom-modeline-icon t)

  (display-time-mode)
  (fancy-battery-mode)
  (doom-modeline-mode))
