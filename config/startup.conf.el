(use-package startup
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-default-init t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))
