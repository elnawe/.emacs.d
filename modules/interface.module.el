;; TODO: Create own mode-line instead of using doom-modeline

(require 'all-the-icons)
(require 'beacon)
(require 'dashboard)

(setq battery-update-interval 15
      column-number-mode t
      display-time-24hr-format t
      display-time-default-load-average nil
      display-time-format "%b %d %H:%M "
      fancy-battery-show-percentage t
      line-number-mode t)

(display-time-mode 1)
(fancy-battery-mode 1)

(with-eval-after-load 'all-the-icons
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (exwm-mode all-the-icons-faicon "laptop")
          (notmuch-hello-mode all-the-icons-faicon "envelope")
          (notmuch-search-mode all-the-icons-faicon "envelope")
          (notmuch-show-mode all-the-icons-faicon "envelope"))))

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

  (setq dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-startup-banner 'logo
        show-week-agenda-p t)

  (setq dashboard-items
        '((bookmarks . 5)
          (projects . 5)
          (agenda . 5)))

  (dashboard-setup-startup-hook))
