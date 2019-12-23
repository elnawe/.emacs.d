(add-to-list 'custom-theme-load-path nemacs-themes-dir)
(add-to-list 'custom-theme-load-path (expand-file-name "c64-theme" nemacs-themes-dir))

(set-fontset-font t 'unicode (font-spec :name "Envy Code R-14") nil)
(set-face-font 'default "Envy Code R-14")

(load-theme 'very-basic t)
