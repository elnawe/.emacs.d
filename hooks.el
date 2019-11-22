(require 'nemacs-utils)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-hook 'find-file-hook #'nemacs-check-large-file)
