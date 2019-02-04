(use-package prog-mode
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-prog-mode ()
    (setq show-trailing-whitespace t)
    (flyspell-prog-mode)
    (flycheck-mode 1))
  :hook (prog-mode . nemacs-prog-mode))

(use-package elisp-mode
  :ensure nil
  :delight emacs-lisp-mode "ξ")

(use-package eldoc
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package js2-mode
  :delight "JS"
  :mode "\\.js\\'"
  :custom (js-indent-level 4))

(use-package json-mode
  :mode "\\.json\\'"
  :custom (js-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom (css-indent-offset 4))

(use-package less-css-mode
  :mode "\\.less\\'"
  :custom (css-indent-offset 4))

(use-package sgml-mode
  :mode "\\.html\\'"
  :custom (sgml-basic-offset 4))
