;;; core/editor.el --- NEMACS CORE Editor File.

;; Copyright (C) 2017 ~ 2021 Nahuel Jesús Sacchetti <me@nsacchetti.com>

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

;;
;;; USING EMACS

;; Variables for large files checks.
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

;; Quit recursively when pressing `C-g'.
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
(global-set-key [remap keyboard-quit] ;; C-g
                #'nemacs-escape)

;; A much better beginning-of-line method.
(defun nemacs-move-beginning-of-line ()
  "Move point to first non-whitespace character, or a beginning
of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))
(global-set-key [remap move-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)

;; Kill current buffer without the prompt
(defun nemacs-kill-current-buffer ()
  "Kill current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key [remap kill-buffer] ;; C-x k
                #'nemacs-kill-current-buffer)

;; Create a window and switch.
(defun nemacs-create-window-bottom-and-switch ()
  "Creates a new window to the bottom and then switch to it."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-below] ;; C-x 2
                #'nemacs-create-window-bottom-and-switch)

;; Same as above, but this time create the window to the right.
(defun nemacs-create-window-right-and-switch ()
  "Creates a new window to the right and then switch to it."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-right] ;; C-x 3
                #'nemacs-create-window-right-and-switch)

;; Check the file size and use `fundamental-mode' if the file is too large.
;; This prevents freeze whenever opening huge text files or images.
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
(add-hook 'find-file-hook #'nemacs-check-large-file)

;;
;;; SAVING FILES

;; Default saving configurations
(setq auto-save-default nil
	  delete-by-moving-to-trash t)
;; Default saving folders
(setq abbrev-file-name (concat nemacs-local-dir "abbrev.el")
      auto-save-list-file-name (concat nemacs-cache-dir "autosave")
      bookmark-default-file (concat nemacs-etc-dir   "bookmarks")
      nsm-settings-file (expand-file-name "ns.data" nemacs-cache-dir)
      pcache-directory (concat nemacs-cache-dir "pcache")
      recentf-save-file (expand-file-name "recentf" nemacs-cache-dir)
      savehist-file (expand-file-name "history" nemacs-cache-dir)
      url-history-file (expand-file-name "url.el" nemacs-cache-dir))
;; After saving, remove all trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;
;;; MINOR MODES

;; Columns and lines are shown in the modeline.
(setq column-number-mode t
      line-number-mode t)
(column-number-mode 1)
;; Show matching parens.
(show-paren-mode t)
;; Revert file to newest version if saved from another system (I.e. changes from VCS).
(global-auto-revert-mode t)
;; Jump word by word instead of jumping the whole block.
(global-subword-mode t)
;; Delete selection when overriden by new code.
(delete-selection-mode t)

;;
;;; PROGRAMMING

(setq-default fill-column 80
	          indent-tabs-mode nil
	          mouse-yank-at-point t
	          require-final-newline t
	          tab-always-indent t
	          tab-width 4
	          tabify-regexp "^\t* [ \t]+"
	          truncate-lines nil)

;; Add TODOs, NOTEs and IMPORTANT keywords
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
;; Temporary face. Change this face within your theme configuration.
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Blue" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
