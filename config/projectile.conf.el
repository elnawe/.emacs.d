(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1)
  :custom
  (projectile-known-projects-file (expand-file-name nemacs-cache-dir "projectile-bookmarks.eld")))
