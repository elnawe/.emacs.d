;; Initializing Packages
(eval-and-compile
  (setq package-user-dir nemacs-packages-dir))

(setq package-enable-at-startup nil)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(add-to-list 'load-path nemacs-elisp-dir)

(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Installing NEMACS
(setq nemacs-necessary-packages
      '(boxquote
        buffer-move
        dashboard
        doom-modeline
        doom-themes
        exwm
        helm
        helm-lastpass
        helm-mu
        json-mode
        markdown-mode
        neotree
        org-bullets
        org-plus-contrib
        projectile
        vterm
        windmove
        xelb
        zerodark-theme))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg nemacs-necessary-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))
