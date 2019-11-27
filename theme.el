(add-to-list 'custom-theme-load-path nemacs-themes-dir)

(load-theme 'zenburn t)

(set-fontset-font t 'unicode (font-spec :name "IBM Plex Mono") nil)
(set-face-font 'default "IBM Plex Mono-11")

(zenburn-with-color-variables
  (custom-set-faces
   `(default ((t (:background ,zenburn-bg-1))))
   `(fringe ((t (:background ,zenburn-bg-05))))
   `(fancy-battery-charging ((t (:bold t :foreground ,zenburn-fg+1))))
   `(fancy-battery-discharging ((t (:bold t :foreground ,zenburn-yellow))))
   `(hl-line ((t (:background ,zenburn-bg+1))))
   `(message-cited-text ((t (:foreground ,zenburn-fg-05))))
   `(message-header-name
     ((t (:bold t :foreground ,zenburn-yellow-2))))
   `(message-header-other ((t (:foreground ,zenburn-fg))))
   `(message-header-subject ((t (:foreground ,zenburn-cyan))))
   `(message-header-to ((t (:foreground ,zenburn-fg+1 :bold t))))
   `(message-separator ((t (:foreground ,zenburn-magenta :italic t))))
   `(mode-line ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow))))
   `(org-agenda-done ((t (:strike-through t))))
   `(org-agenda-structure
     ((t (:foreground ,zenburn-fg+1 :height 200 :underline t))))
   `(org-ellipsis ((t (:bold t :foreground ,zenburn-fg :underline nil))))
   `(org-headline-done ((t (:strike-through t))))
   `(org-level-1 ((t (:bold t :foreground ,zenburn-fg))))
   `(org-todo ((t (:underline t))))
   `(org-tag ((t (:foreground ,zenburn-cyan))))
   `(powerline-active0
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(powerline-active1
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(powerline-active2
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(region ((t (:background ,zenburn-green :foreground ,zenburn-bg-2))))
   `(variable-pitch ((t (:height 60))))

   `(mode-line-buffer-id-inactive ((t (:foreground ,zenburn-fg-05))))
   `(mode-line-inactive
     ((t :background ,zenburn-bg :foreground ,zenburn-fg-05)))
   `(powerline-inactive0
     ((t :background ,zenburn-bg :foreground ,zenburn-fg-05)))
   `(powerline-inactive1
     ((t :background ,zenburn-bg :foreground ,zenburn-fg-05)))
   `(powerline-inactive2
     ((t :background ,zenburn-bg :foreground ,zenburn-fg-05)))))
