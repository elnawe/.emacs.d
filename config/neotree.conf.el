(use-package neotree
  :bind
  (("C-x C-d" . neotree-toggle)
   ("<f2>"    . neotree-toggle))
  :custom
  (neo-create-file-auto-open t)
  (neo-hide-cursor t)
  (neo-reset-size-on-open t)
  (neo-smart-open t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-window-position 'right)
  (neo-window-width 40))
