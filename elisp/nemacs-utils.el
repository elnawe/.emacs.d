;;; nemacs-utils.el --- NEMACS Utilities.

;; Utilities used in NEMACS, like a new keyboard-quit version that can be
;; hooked, check for file size before opening it (and open it in
;; `fundamental-mode' if the file is too big), and more.

;; Copyright (C) 2017 ~ 2019 Nahuel Jesús Sacchetti <me@nsacchetti.com>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar nemacs-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the
    file literally to avoid performance issues. Opening literally
    means that no major or minor modes are active and the buffer
    is read-only.")

(defvar nemacs-large-file-modes-list '(archive-mode
                                       tar-mode
                                       jka-compr
                                       git-commit-mode
                                       image-mode
                                       doc-view-mode
                                       doc-view-mode-maybe
                                       ebrowse-tree-mode
                                       pdf-view-mode)
  "Major modes that `nemacs/check-large-file' will ignore")

(defvar nemacs-escape-hook nil
  "A hook that runs after pressing `C-g' and before
    `keyboard-quit'")

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
  "Move point to first non-whitespace character, or a beginning
of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))

(defun nemacs-kill-current-buffer ()
  "Kill current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun nemacs-check-large-file ()
  "Check if the buffer's file is large (see `nemacs-large-file-size'). If so,
ask for confirmation to open it literally (read-only, disable undo and in
fundamental-mode) for performance sake."
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
  "Prompts before closing the frame with `C-x C-c'. Standarizes `emacs' and
`emacsclient'."
  (interactive)
  (if (y-or-n-p ">>> Quit Nemacs? ")
      (save-buffers-kill-terminal)
    (message "Good. You should never do it.")))

(provide 'nemacs-utils)
