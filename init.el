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

(when IS-LINUX
  (set-fontset-font t 'unicode (font-spec :name "Envy Code R-12") nil)
  (set-face-font 'default "Envy Code R-12"))

(when IS-WINDOWS
  (set-fontset-font t 'unicode (font-spec :name "DejaVu Sans Mono-12") nil)
  (set-face-font 'default "DejaVu Sans Mono-12"))

;;
;;; THEME

(use-package zenburn-theme
  :custom
  (zenburn-use-variable-pitch nil)
  (zenburn-scale-org-headlines nil)
  (zenburn-scale-outline-headlines nil)
  (zenburn-override-colors-alist
   '(;; background
     ("zenburn-bg"      . "#2A282A")
     ;; foreground
     ("zenburn-fg"      . "#D0BF8F")
     ;; mode-line
     ("zenburn-orange"  . "#AB8368")
     ;; comments
     ("zenburn-yellow"  . "#F7E743")
     ;; strings
     ("zenburn-fg-1"    . "#999999")
     ;; types
     ("zenburn-green+4" . "#4AC981")
     ;; constants
     ("zenburn-cyan"    . "#4AC9C1")))
  :config
  (load-theme 'zenburn t)

  (zenburn-with-color-variables
    (custom-set-faces
     ;; Default and GUI
     `(cursor ((t (:background ,zenburn-green+4))))
     `(default ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))
     `(region ((t (:background "#0000FF"))))
     `(fringe ((t (:background ,zenburn-bg))))
     `(mode-line ((t (:background ,zenburn-orange
                                  :foreground ,zenburn-bg-2
                                  :height 100
                                  :bold nil
                                  :box nil))))
     `(mode-line-buffer-id ((t (:foreground ,zenburn-bg-2))))
     `(mode-line-inactive ((t (:background ,zenburn-fg-1
                                           :foreground ,zenburn-bg-1
                                           :height 100
                                           :box nil))))
     `(org-level-1 ((t (:foreground ,zenburn-fg+2 :bold t))))

     ;; Search
     `(anzu-mode-line ((t (:foreground ,zenburn-bg-2))))

     ;; Comments
     `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-yellow))))
     `(font-lock-comment-face  ((t (:foreground ,zenburn-yellow))))

     ;; Programming
     ;; static or emacs :foreground | same as 'default
     `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :bold nil))))
     `(typescript-access-modifier-face ((t (:foreground ,zenburn-fg :bold nil))))
     `(typescript-this-face ((t (:foreground ,zenburn-fg :bold nil))))
     `(css-property ((t (:foreground ,zenburn-fg :bold nil))))
     `(font-lock-type-face ((t (:foreground ,zenburn-fg))))
     ;; function name | same as 'default
     `(font-lock-function-name-face ((t (:foreground ,zenburn-fg))))
     ;; variable name | same as 'default
     `(font-lock-variable-name-face ((t (:foreground ,zenburn-fg))))
     ;; constant value | cyan
     `(font-lock-constant-face ((t (:foreground ,zenburn-cyan))))
     ;; type | green
     `(typescript-primitive-face ((t (:foreground ,zenburn-green+4))))
     ;; if, for, return and more | white
     `(font-lock-keyword-face ((t (:foreground ,zenburn-fg+2 :bold nil))))
     ;; #include | blue
     `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue))))
     ;; strings and docs | gray
     `(font-lock-doc-face ((t (:foreground ,zenburn-fg-1))))
     `(font-lock-string-face ((t (:foreground ,zenburn-fg-1))))
     ;; Todos, Importants and Notes
     `(font-lock-fixme-face ((t (:foreground ,zenburn-red-2 :bold t :underline t))))
     `(font-lock-important-face ((t (:foreground ,zenburn-blue :bold t :underline t))))
     `(font-lock-note-face ((t (:foreground ,zenburn-green :bold t :underline t)))))

    ;; Extra borders for the mode-line
    (set-face-attribute 'mode-line nil
                        :box '(:line-width 5 :color "#AB8368"))
    (set-face-attribute 'mode-line-inactive nil
                        :box '(:line-width 5 :color "#999999"))))

;;;
;;   PACKAGES
;;;

;;
;;; ANZU

(use-package anzu
  :config
  (global-anzu-mode +1))

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
    ;; Define indentation style
    (defconst nemacs-c-is-fun-style
      '((c-electric-pound-behavior   . nil)
        (c-tab-always-indent         . t)
        (c-comment-only-line-offset  . 0)
        (c-hanging-braces-alist      . ((class-open)
                                        (class-close)
                                        (defun-open)
                                        (defun-close)
                                        (inline-open)
                                        (inline-close)
                                        (brace-list-open)
                                        (brace-list-close)
                                        (brace-list-intro)
                                        (brace-list-entry)
                                        (block-open)
                                        (block-close)
                                        (substatement-open)
                                        (statement-case-open)
                                        (class-open)))
        (c-hanging-colons-alist      . ((inher-intro)
                                        (case-label)
                                        (label)
                                        (access-label)
                                        (access-key)
                                        (member-init-intro)))
        (c-cleanup-list              . (scope-operator
                                        list-close-comma
                                        defun-close-semi))
        (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                        (label                 . -4)
                                        (access-label          . -4)
                                        (substatement-open     .  0)
                                        (statement-case-intro  .  4)
                                        (case-label            .  4)
                                        (block-open            .  0)
                                        (inline-open           .  0)
                                        (topmost-intro-cont    .  0)
                                        (knr-argdecl-intro     . -4)
                                        (brace-list-open       .  0)
                                        (brace-list-intro      .  4))))
      "NEMACS' C-is-fun Style mode based on Casey Muratori's style.")

    (defun nemacs-c++-hook ()
      (c-add-style "C-is-Fun" nemacs-c-is-fun-style t)

      (setq tab-width 4
            indent-tabs-mode nil)

      (c-set-offset 'member-init-intro '++)
      (c-toggle-auto-hungry-state -1))

    ;; Build and run C projects with their Makefile
    (defun nemacs-cc-compile ()
      (interactive)
      (let ((cd-project-dir (concat "cd " (projectile-project-root))))
        (if IS-LINUX
            (compile (concat cd-project-dir " && ./build.sh"))
          (compile (concat cd-project-dir " && build.bat")))))

    (defun nemacs-cc-run ()
      (interactive)
      (let ((cd-project-dir (concat "cd " (projectile-project-root)))
            (display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
                                        (cons #'display-buffer-no-window nil)))))
        (if IS-LINUX
            (async-shell-command (concat cd-project-dir " && ./run.sh"))
          (async-shell-command (concat cd-project-dir " && run.bat")))))

    (defun nemacs-cc-compile-and-run ()
      (interactive)
      (nemacs-cc-compile)
      (nemacs-cc-run))
    :bind
    (:map c-mode-map
          ("<f6>" . nemacs-cc-compile)
          ("<f7>" . nemacs-cc-run)
          ("<f8>" . nemacs-cc-compile-and-run))
    (:map c++-mode-map
          ("<f6>" . nemacs-cc-compile)
          ("<f7>" . nemacs-cc-run)
          ("<f8>" . nemacs-cc-compile-and-run)
          ("C-m"  . newline-and-indent))
    :hook
    (cc-mode  . nemacs-c++-hook)
    (c++-mode . nemacs-c++-hook)
    :custom
    (c-basic-offset 4))

  (use-package js2-mode
    :custom
    (js-indent-level 4))

  (use-package json-mode)

  (use-package typescript-mode
    :custom
    (typescript-indent-level 4))

  (use-package rjsx-mode
    :mode "/feedback-analysis/"))

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
