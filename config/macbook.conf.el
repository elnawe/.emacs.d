;; MacBook specific configurations. Move the META to the Command key and set `fn-delete' as backwards delete.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :bind (([kp-delete] . delete-char))
    :init
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "path")
    :custom
    (mac-option-modifier 'alt)
    (mac-command-modifier 'meta)))
