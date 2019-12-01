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

  (setq projectile-globally-ignored-directories '(".idea"
                                                  ".ensime_cache"
                                                  ".eunit"
                                                  ".git"
                                                  ".hg"
                                                  ".fslckout"
                                                  "_FOSSIL_"
                                                  ".bzr"
                                                  "_darcs"
                                                  ".tox"
                                                  ".svn"
                                                  ".stack-work"
                                                  "node_modules"
                                                  ".local"))

  (helm-projectile-on)
  (projectile-mode))
