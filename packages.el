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
      '(all-the-icons
        beacon
        boxquote
        buffer-move
        company
        dashboard
        exwm
        fancy-battery
        helm
        helm-exwm
        helm-lastpass
        helm-projectile
        idle-org-agenda
        json-mode
        ledger-mode
        markdown-mode
        mu4e-alert
        neotree
        night-owl-theme
        org-bullets
        org-plus-contrib
        org-super-agenda
        projectile
        restart-emacs
        rust-mode
        vterm
        windmove
        xelb))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg nemacs-necessary-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))
