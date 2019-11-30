(require 'projectile)

(with-eval-after-load 'projectile
  (require 'helm-projectile)

  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

  (setq projectile-enable-caching t)

  (setq projectile-cache-file (expand-file-name
                               "projectile.cache"
                               nemacs-cache-dir))

  (setq projectile-known-projects-file (expand-file-name
                                        "projectile-bookmarks"
                                        nemacs-cache-dir))

  (helm-projectile-on)
  (projectile-mode))
