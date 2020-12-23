;;; init.el --- NEMACS Initialization File.

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

(load (concat user-emacs-directory "core/core.el"))

(nemacs-initialize)

;;
;;; FONTS

(set-fontset-font t 'unicode (font-spec :name "Envy Code R-12") nil)
(set-face-font 'default "Envy Code R-12")

;;
;;; THEME

(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "#808bed")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#cfbfad")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "#808bed")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-foreground-color "#cfbfad")
(set-background-color "#1e1e27")
(set-cursor-color "#40FF40")

;;;
;;   PACKAGES
;;;

;;
;;; HELM

(use-package helm
  :preface
  (defun nemacs-setup-helm-mode ()
    "NEMACS Setup: Run this function in `helm-mode-hook'."
    (setq-local line-spacing 0.2))
  :bind
  (([remap find-file]                . helm-find-files)
   ([remap execute-extended-command] . helm-M-x)
   ([remap switch-to-buffer]         . helm-mini)
   ([remap occur]                    . helm-occur)
   ([remap apropos-command]          . helm-apropos)
   ([remap yank-pop]                 . helm-show-kill-ring)
   :map helm-find-files-map
   ("C-<backspace>" . backward-kill-word)
   :map helm-map
   ("TAB" . helm-maybe-exit-minibuffer)
   ("C-<backspace>" . backward-kill-word))
  :hook
  (helm-major-mode . nemacs-setup-helm-mode)
  :custom
  (helm-boring-buffer-regexp-list
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
   helm-reuse-last-window-split-state t)
  :config
  (require 'helm-config)

  (helm-mode t))

;;
;;; MAGIT

(use-package magit
  :custom
  (transient-history-file (concat nemacs-cache-dir "transient/history.el")))

;;
;;; PROGRAMMING

(use-package prog-mode
  :ensure nil
  :custom
  ;; Html tab width
  (sgml-offset 4)
  :config
  (use-package cc-mode
    ;; Programming mode for C language
    :ensure nil
    :preface
    ;; Build and run C projects with their Makefile
    (defun nemacs-cc-compile ()
      (interactive)
      (let ((cd-project-dir (concat "cd " (projectile-project-root))))
        (if IS-LINUX
            (compile (concat cd-project-dir " && ./build.sh"))
          (compile (concat cd project-dir " && build.bat")))))

    (defun nemacs-cc-run ()
      (interactive)
      (let ((cd-project-dir (concat "cd " (projectile-project-root)))
            (display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
                                        (cons #'display-buffer-no-window nil)))))
        (if IS-LINUX
            (async-shell-command (concat cd-project-dir " && ./run.sh"))
          (async-shell-command (concat cd project-dir " && run.bat")))))

    (defun nemacs-cc-compile-and-run ()
      (interactive)
      (nemacs-cc-compile)
      (nemacs-cc-run))
    :bind
    (:map c-mode-map
          ("<f6>" . nemacs-cc-compile)
          ("<f7>" . nemacs-cc-run)
          ("<f8>" . nemacs-cc-compile-and-run))
    :custom
    (c-basic-offset 4))

  (use-package js2-mode
    :custom
    (js-indent-level 4))

  (use-package json-mode)

  (use-package typescript-mode
    :custom
    (typescript-indent-level 4)
    :custom-face
    (typescript-this-face ((t (:inherit 'font-lock-builtin-face))))))

;;
;;; PROJECTILE

(use-package projectile
  :custom
  ;; Enable cache
  (projectile-enabled-caching t)
  (projectile-cache-file (expand-file-name "projectile.cache" nemacs-cache-dir))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks" nemacs-cache-dir))
  ;; Ignore directories
  (projectile-globally-ignored-directories '(".idea"
                                             ".ensime_cache"
                                             ".eunit"
                                             ".git"
                                             ".hg"
                                             ".fslckout"
                                             "_FOSSIL_"
                                             ".bzr"
                                             "_darcs"
                                             ".tox"
                                             ".svn"
                                             ".stack-work"
                                             "angle-template"
                                             "node_modules"
                                             ".local"))
  :config
  ;; Install helm-projectile
  (use-package helm-projectile
    :config
    (helm-projectile-on))

  ;; Define `projectile' master key
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

  ;; Ignore directories
  (projectile-mode))

;;
;;; VTERM

(use-package vterm
  :if IS-LINUX
  :bind
  (("C-x t" . nemacs-create-switch-to-vterm)
   ("C-x T" . nemacs-create-switch-to-vterm-other-window))
  :config
  ;; Check for `cmake' system dependency before continuing.
  (nemacs-ensure-system-package "cmake" t)

  (defun nemacs-create-switch-to-vterm ()
    "Switch to `vterm' buffer if exists.
If not, automatically creates one."
    (interactive)
    (if (buffer-live-p (get-buffer "vterm"))
        (switch-to-buffer "vterm")
      (vterm)))

  (defun nemacs-create-switch-to-vterm-other-window ()
    "Split window and switch to `vterm' buffer if exists.
If not, automatically creates one."
    (interactive)
    (if (buffer-live-p (get-buffer "vterm"))
        (progn (nemacs-create-window-right-and-switch)
               (switch-to-buffer "vterm"))
      (vterm-other-window))))
