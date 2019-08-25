(use-package helm
  :init
  (require 'helm-config)
  (helm-mode 1)
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x"     . helm-M-x))
  :custom-face
  (helm-ff-directory ((t (:bold t)))))
