(add-to-list 'custom-theme-load-path nemacs-themes-dir)

(set-fontset-font t 'unicode (font-spec :name "Envy Code R-14") nil)
(set-face-font 'default "Envy Code R-14")

(load-theme 'night-owl t)
