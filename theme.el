(add-to-list 'custom-theme-load-path nemacs-themes-dir)

(require 'doom-themes)

(with-eval-after-load 'doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-dracula t)

  (set-fontset-font t 'unicode (font-spec :name "IBM Plex Mono") nil)
  (set-face-font 'default "IBM Plex Mono-11"))
