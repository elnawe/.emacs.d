;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-mono-dark t)
;;   ;;(doom-themes-org-config)
;;   ;;(doom-themes-neotree-config))
;;   )

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-enable-word-count t))
