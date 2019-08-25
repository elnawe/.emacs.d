(use-package simple
  :ensure nil
  :preface
  (defvar nemacs-large-file-size 1
    "Size (in MB) above which the user will be prompted to open the file literally to avoid performance issues. Opening literally means that no major or minor modes are active and the buffer is read-only.")

  (defvar nemacs-large-file-modes-list
    '(archive-mode tar-mode jka-compr git-commit-mode image-mode doc-view-mode
                   doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
    "Major modes that `nemacs/check-large-file' will ignore")

  (defvar nemacs-escape-hook nil
    "A hook that runs after pressing `C-g' and before `keyboard-quit'")

  (defun nemacs-escape ()
    "Runs the `nemacs-escape-hook'."
    (interactive)
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'nemacs-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; runs the default quit.
          (t (keyboard-quit))))

  (defun nemacs-move-beginning-of-line ()
    "Move point to first non-whitespace character, or a beginning of line."
    (interactive "^")
    (let ((origin (point)))
      (beginning-of-line)
      (and (= origin (point))
           (back-to-indentation))))

  (defun nemacs-kill-current-buffer ()
    "Kill current buffer without prompting"
    (interactive)
    (kill-buffer (current-buffer)))

  (defun nemacs-check-large-file ()
    "Check if the buffer's file is large (see `nemacs-large-file-size'). If so, ask for confirmation to open it literally (read-only, disable undo and in fundamental-mode) for performance sake."
    (let* ((filename (buffer-file-name))
           (size (nth 7 (file-attributes filename))))
      (when (and (not (memq major-mode nemacs-large-file-modes-list))
                 size (> size (* 1024 1024 nemacs-large-file-size))
                 (y-or-n-p
                  (format (concat "%s is a large file, open literally to "
                                  "avoid performance issues?")
                          (file-relative-name filename))))
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode))))

  (defun nemacs-create-window-bottom-and-switch ()
    "Creates a new window to the bottom and then switch to it"
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun nemacs-create-window-right-and-switch ()
    "Creates a new window to the right and then switch to it"
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun nemacs-prompt-before-exiting-emacs ()
    "Prompts before closing the frame with `C-x C-c'. Standarizes `emacs' and `emacsclient'."
    (interactive)
    (if (y-or-n-p ">>> Quit Nemacs? ")
        (save-buffers-kill-terminal)
      (message "Good. You should never do it.")))
  :hook
  ((find-file   . nemacs-check-large-file))
  :bind
  (([remap save-buffers-kill-emacs]    . nemacs-prompt-before-exiting-emacs)
   ([remap save-buffers-kill-terminal] . nemacs-prompt-before-exiting-emacs)
   ([remap suspend-frame]              . ignore)
   ([remap split-window-right]         . nemacs-create-window-right-and-switch)
   ([remap split-window-below]         . nemacs-create-window-bottom-and-switch)
   ([remap kill-buffer]                . nemacs-kill-current-buffer)
   ([remap keyboard-quit]              . nemacs-escape)
   ([remap move-beginning-of-line]     . nemacs-move-beginning-of-line)
   ([remap org-beginning-of-line]      . nemacs-move-beginning-of-line)))

(use-package subword
  :ensure nil
  :delight)
