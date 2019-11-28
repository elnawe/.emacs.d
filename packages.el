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
        doom-modeline
        exwm
        fancy-battery
        fill-column-indicator
        helm
        helm-exwm
        helm-lastpass
        idle-org-agenda
        json-mode
        ledger-mode
        markdown-mode
        neotree
        org-bullets
        org-plus-contrib
        org-super-agenda
        projectile
        vterm
        windmove
        xelb
        zenburn-theme))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg nemacs-necessary-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))
