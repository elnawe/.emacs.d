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
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburn-fg-05))))
   `(gnus-summary-normal-ticked ((t (:bold t :foreground ,zenburn-yellow))))
   `(helm-candidate-number
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg))))
   `(hl-line ((t (:background ,zenburn-bg))))
   `(message-cited-text ((t (:foreground ,zenburn-fg-05))))
   `(message-header-name
     ((t (:bold t :foreground ,zenburn-yellow-2))))
   `(message-header-other ((t (:foreground ,zenburn-fg))))
   `(message-header-subject ((t (:foreground ,zenburn-cyan))))
   `(message-header-to ((t (:foreground ,zenburn-fg+1 :bold t))))
   `(message-separator ((t (:foreground ,zenburn-magenta :italic t))))
   `(mode-line
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1 :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow))))
   `(notmuch-tag-face ((t (:foreground ,zenburn-magenta))))
   `(org-agenda-date ((t (:foreground ,zenburn-fg-05 :height 120))))
   `(org-agenda-date-today
     ((t (:foreground ,zenburn-fg :height 150 :italic nil))))
   `(org-agenda-date-weekend ((t (:bold t :foreground ,zenburn-fg-05))))
   `(org-agenda-done ((t (:strike-through t))))
   `(org-agenda-structure
     ((t (:bold t :foreground ,zenburn-fg+1 :height 200))))
   `(org-ellipsis ((t (:bold t :foreground ,zenburn-fg :underline nil))))
   `(org-headline-done ((t (:foreground ,zenburn-fg :strike-through t))))
   `(org-level-1 ((t (:bold t :foreground ,zenburn-fg))))
   `(org-level-2 ((t (:foreground ,zenburn-fg :italic t))))
   `(org-level-3 ((t (:foreground ,zenburn-fg :italic t))))
   `(org-recur ((t (:bold t :box t :foreground ,zenburn-fg-05))))
   `(org-scheduled ((t (:foreground ,zenburn-fg))))
   `(org-scheduled-previously ((t (:foreground ,zenburn-red+2 :italic t))))
   `(org-scheduled-today ((t (:foreground ,zenburn-fg+1))))
   `(org-tag ((t (:foreground ,zenburn-cyan))))
   `(org-time-grid ((t (:foreground ,zenburn-green+4 :height 90))))
   `(org-todo ((t (:underline t))))
   `(powerline-active0
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(powerline-active1
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(powerline-active2
     ((t (:background ,zenburn-green-5 :foreground ,zenburn-fg+1))))
   `(region ((t (:background ,zenburn-green :foreground ,zenburn-bg-2))))
   `(variable-pitch ((t (:height 100))))

   `(mode-line-buffer-id-inactive ((t (:foreground ,zenburn-fg-05))))
   `(mode-line-inactive
     ((t (:background ,zenburn-bg :foreground ,zenburn-fg-05 :box nil))))
   `(powerline-inactive0
     ((t (:background ,zenburn-bg :foreground ,zenburn-fg-05))))
   `(powerline-inactive1
     ((t (:background ,zenburn-bg :foreground ,zenburn-fg-05))))
   `(powerline-inactive2
     ((t (:background ,zenburn-bg :foreground ,zenburn-fg-05))))))
