(use-package helm
  :init
  (require 'helm-config)
  (helm-mode 1)
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x"     . helm-M-x)
   ("C-x b"   . helm-mini))
  :custom
  (helm-boring-buffer-regexp-list '(;; Helm buffers
                                    "\\` " "\\*helm" "\\*helm-mode"
                                    ;; Emacs buffers
                                    "\\*Echo Area" "\\*Minibuf" "\\*Compile-Log\\*"
                                    "\\*dashboard\\*" "\\*scratch\\*" "\\*Help\\*" "tramp/.+"
                                    "\\*Messages\\*" "\\*Flycheck error" "\\*.+(.+)" "elpa/.+"
                                    ;; Magit buffers
                                    "\\*magit-process:" "\\*magit-diff:"))
  (helm-ff-skip-boring-files t)
  :custom-face
  (helm-ff-directory ((t (:bold t)))))
