;; Ignore messages during startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Defaults
(setq-default auto-save-default nil
              bidi-display-reordering nil
              blink-matching-paren nil
              buffer-file-coding-system  'utf-8
              cursor-in-non-selected-windows nil
              custom-file (expand-file-name "custom.el" nemacs-etc-dir)
              create-lockfiles nil
              delete-by-moving-to-trash t
              fill-column 80
              frame-inhibit-implied-resize t
              frame-title-format "NEMACS"
              help-window-select t
              highlight-nonselected-windows nil
              fringe-indicator-alist (delq
                                      (assq 'continuation fringe-indicator-alist)
                                      fringe-indicator-alist)
              indent-tabs-mode nil
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              make-backup-files nil
              max-mini-window-height 0.3
              mode-line-default-help-echo nil
              mouse-yank-at-point t
              ns-right-alternate-modifier nil
              require-final-newline t
              resize-mini-windows 'grow-only
              ring-bell-function #'ignore
              show-help-function nil
              split-height-threshold nil
              split-width-threshold 160
              tab-always-indent t
              tab-width 4
              tabify-regexp "^\t* [ \t]+"
              truncate-lines nil
              uniquify-buffer-name-style 'post-forward-angle-brackets
              use-dialog-box nil
              use-package-always-ensure t
              vc-handled-backends nil
              visible-bell nil
              visible-cursor nil
              whitespace-line-column fill-column
              whitespace-style '(face tab trailing)
              word-wrap t
              x-stretch-cursor t)

;; Save files
(setq-default auto-save-list-file-name (concat nemacs-cache-dir "autosave")
              bookmark-default-file    (concat nemacs-etc-dir   "bookmarks")
              abbrev-file-name         (concat nemacs-local-dir "abbrev.el")
              pcache-directory         (concat nemacs-cache-dir "pcache")
              recentf-save-file        (expand-file-name "recentf" nemacs-cache-dir))

;; Simpler UI
(when window-system
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Modes and default folder
(cd "~")
(fset #'yes-or-no-p #'y-or-n-p)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(column-number-mode t)

;; Memory Management
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq max-lisp-eval-depth 50000
      max-specpdl-size 13000)

;; Load Modules Configuration
(setq load-prefer-newer noninteractive)

;; Startup
(setq inhibit-default-init t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-startup-message t)
