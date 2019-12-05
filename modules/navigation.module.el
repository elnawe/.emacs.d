(require 'helm)

(with-eval-after-load 'helm
  (require 'helm-config)
  (require 'helm-lastpass)

  (defun nemacs-setup-helm-mode ()
    "NEMACS Setup: Run this function in `helm-mode-hook'."
    (setq-local line-spacing 0.2))

  (helm-mode t)

  (add-hook 'helm-major-mode-hook #'nemacs-setup-helm-mode)

  (define-key helm-map (kbd "TAB") #'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "C-<backspace>") #'backward-kill-word)
  (define-key helm-find-files-map (kbd "C-<backspace>") #'backward-kill-word)

  (global-set-key [remap find-file] #'helm-find-files)
  (global-set-key [remap execute-extended-command] #'helm-M-x)
  (global-set-key [remap switch-to-buffer] #'helm-mini)
  (global-set-key [remap occur] #'helm-occur)
  (global-set-key [remap apropos-command] #'helm-apropos)
  (global-set-key [remap yank-pop] #'helm-show-kill-ring)

  (setq helm-boring-buffer-regexp-list
        '(;; Helm buffers
          "\\` " "\\*helm" "\\*helm-mode"
          ;; Emacs buffers
          "\\*Echo Area" "\\*Minibuf" "\\*Compile-Log\\*"
          "\\*Backtrace\\*" "\\*dashboard\\*" "\\*scratch\\*"
          "\\*Help\\*" "tramp/.+" "\\*Flycheck error"
          "\\*Messages\\*" "\\*.+(.+)" "elpa/.+"
          ;; Magit buffers
          "\\*magit-process:" "\\*magit-diff:")
        helm-dwim-target 'completion
        helm-echo-input-in-header-line t
        helm-ff-skip-boring-files t
        helm-reuse-last-window-split-state t))
