(require 'company)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)

  (setq company-idle-delay 0.5
        company-minimum-prefix-length 3)

  (add-hook 'prog-mode-hook #'company-mode))
