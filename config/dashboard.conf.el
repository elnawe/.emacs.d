(use-package dashboard
  :preface
  (defun nemacs-dashboard-banner ()
    (setq dashboard-banner-logo-title
          (format "NEMACS ready in %.2f seconds with %d garbage collections.\n"
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :hook
  ((after-init     . dashboard-refresh-buffer)
   (dashboard-mode . nemacs-dashboard-banner))
  :custom
  (dashboard-startup-banner 'logo)
  :init
  (dashboard-setup-startup-hook))
