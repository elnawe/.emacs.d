(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :bind (:map flyspell-mode-map
              ("M-t" . nil)))
