(require 'nemacs-utils)

(global-set-key [remap save-buffers-kill-emacs] ;; C-x C-c
                #'nemacs-prompt-before-exiting-emacs)

(global-set-key [remap save-buffers-kill-terminal] ;; C-x C-c
                #'nemacs-prompt-before-exiting-emacs)

(global-set-key [remap suspend-frame] ;; C-z
                #'ignore)

(global-set-key [remap split-window-right] ;; C-x 3
                #'nemacs-create-window-right-and-switch)

(global-set-key [remap split-window-below] ;; C-x 2
                #'nemacs-create-window-bottom-and-switch)

(global-set-key [remap kill-buffer] ;; C-x k
                #'nemacs-kill-current-buffer)

(global-set-key [remap keyboard-quit] ;; C-g
                #'nemacs-escape)

(global-set-key [remap move-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)

(global-set-key [remap org-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)
