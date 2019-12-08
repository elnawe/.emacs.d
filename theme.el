(add-to-list 'custom-theme-load-path (expand-file-name "c64-theme" nemacs-themes-dir))

(set-fontset-font t 'unicode (font-spec :name "Bitstream Vera Sans Mono-12") nil)
(set-face-font 'default "Bitstream Vera Sans Mono-12")

(load-theme 'c64 t)
