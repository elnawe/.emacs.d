(require 'company)
(require 'markdown-mode)
(require 'rust-mode)
(require 'rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode #'flow-minor-enable-automatically)
(add-hook 'js-mode #'flow-minor-enable-automatically)

(setq js-indent-level 4
      sgml-basic-offset 4)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)

  (setq company-idle-delay 0.5
        company-minimum-prefix-length 3)

  (add-hook 'prog-mode-hook #'company-mode))
