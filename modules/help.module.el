(require 'help)

(with-eval-after-load 'help
  (defun nemacs-setup-help-mode ()
    "NEMACS Setup: Run this function in `help-mode-hook'."
    (local-set-key (kbd "C-f") #'find-function)
    (local-set-key (kbd "C-k") #'find-function-on-key)
    (local-set-key (kbd "C-l") #'find-library)
    (local-set-key (kbd "C-v") #'find-variable))

  (add-hook 'help-mode-hook #'nemacs-setup-help-mode))
