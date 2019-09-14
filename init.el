;;; init.el --- NEMACS Initialization File.

;; Copyright (C) 2017 ~ 2019 Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Ignore all startup messages
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; NEMACS variables
(eval-and-compile
  (defvar nemacs-shared-dir "~/Shared/")

  (defvar nemacs-dotfiles-dir (concat nemacs-shared-dir "dotfiles/"))

  (defvar nemacs-emacs-dir (expand-file-name user-emacs-directory))

  (defvar nemacs-local-dir (concat nemacs-emacs-dir ".local/"))

  (defvar nemacs-config-dir (concat nemacs-emacs-dir "config/"))

  (defvar nemacs-df-config-dir (concat nemacs-dotfiles-dir "elisp"))

  (defvar nemacs-config-file-list '())

  (defvar nemacs-packages-dir (concat nemacs-local-dir "packages/"))

  (defvar nemacs-cache-dir (concat nemacs-local-dir "cache/"))

  (defvar nemacs-etc-dir (concat nemacs-local-dir "etc/"))

  (defvar nemacs-enable-extras t
    "When nil, disable all the packages. Change this value in `nemacs-custom.el'")

  (dolist (dir (list
                nemacs-cache-dir
                nemacs-config-dir
                nemacs-local-dir
                nemacs-etc-dir
                (expand-file-name "elpa" nemacs-packages-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

;; Default settings
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

;; Default directories and files
(setq-default auto-save-list-file-name (concat nemacs-cache-dir "autosave")
              bookmark-default-file    (concat nemacs-etc-dir   "bookmarks")
              abbrev-file-name         (concat nemacs-local-dir "abbrev.el")
              pcache-directory         (concat nemacs-cache-dir "pcache")
              recentf-save-file        (expand-file-name "recentf" nemacs-cache-dir))

(when window-system
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(cd "~")
(fset #'yes-or-no-p #'y-or-n-p)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(column-number-mode t)
(set-fontset-font t 'unicode (font-spec :name "IBM Plex Mono") nil)
(set-face-font 'default "IBM Plex Mono-11")

;; Packages and memory
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)
  (setq package-user-dir (expand-file-name "elpa" nemacs-packages-dir)))

(setq max-lisp-eval-depth 50000
      max-specpdl-size 10000)

(setq load-prefer-newer noninteractive
      package-enable-at-startup nil)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package delight)
(use-package use-package-ensure-system-package)

;; Create a list of configuration files
(dolist (file (directory-files nemacs-config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (add-to-list 'nemacs-config-file-list (expand-file-name file nemacs-config-dir) t)))

(dolist (file (directory-files nemacs-df-config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (add-to-list 'nemacs-config-file-list (expand-file-name file nemacs-df-config-dir) t)))


;; Initialization
(add-hook 'after-init-hook
          #'(lambda ()
              ;; Load configuration files (if `nemacs-enable-extras')
              (when nemacs-enable-extras
                (mapc (lambda (file)
                        (load file))
                      nemacs-config-file-list))

              ;; Reset defaults
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))
