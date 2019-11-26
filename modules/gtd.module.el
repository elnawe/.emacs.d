;; TODO: org-agenda to open projects based on TAGS

(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-capture)
(require 'org-id)

(with-eval-after-load 'org
  (defvar nemacs-org-dir (concat nemacs-dropbox-dir "Notes/")
    "Default directory for all the Org files related to notes.")

  (defvar nemacs-org-todo-file ""
    "File where all my TODOs are saved and managed.")

  (defun nemacs-org-file (filename)
    "Expands from `nemacs-org-dir', appending `filename'."
    (expand-file-name filename nemacs-org-dir))

  (defun nemacs-setup-org-mode ()
    "NEMACS Setup: Run this function in `org-mode-hook'"
    (flyspell-mode)
    (org-bullets-mode)
    (org-indent-mode)
    (turn-on-visual-line-mode)
    (setq-local line-spacing 0.1))

  (add-hook 'org-mode-hook #'nemacs-setup-org-mode)

  (global-set-key (kbd "C-c l") #'org-store-link)

  (setq nemacs-org-todo-file (nemacs-org-file "todo.org")
        org-archive-location (nemacs-org-file "archive.org::datetree/")
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-deadline-warning-days 7
        org-default-notes-file (nemacs-org-file "inbox.org")
        org-descriptive-links t
        org-directory nemacs-org-dir
        org-ellipsis " […]"
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-startup-folded nil
        org-startup-truncated nil
        org-support-shift-always 'always
        org-tags-column -120)

  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets org-agenda-files
        org-refile-use-outline-path t)

  (setq org-tag-persistent-alist
        '(;; Action
          ("%BUG"    . ?B)
          ("%STORY"  . ?S)
          ("%TRIAGE" . ?T)
          ("%WIKI"   . ?W)

         ;; Context
         ("@computer" . ?c)
         ("@emacs"    . ?e)
         ("@gameroom" . ?g)
         ("@home"     . ?h)
         ("@meeting"  . ?m)
         ("@office"   . ?o)

         ;; Projects
         ("#OK"  . ?O)
         ("#ROR" . ?R)))

 (setq org-todo-keywords
       '((sequence "TODO(t!)"
                   "CLEARED(c@)"
                   "PROJECT(p@)"
                   "WAITING(w@)"
                   "|"
                   "DONE(D@)"
                   "CANCELED(C@)")))

 (zenburn-with-color-variables
   (setq org-todo-keyword-faces
         `(("TODO"     . ,zenburn-orange)
           ("CLEARED"  . ,zenburn-yellow)
           ("PROJECT"  . ,zenburn-magenta)
           ("WAITING"  . ,zenburn-cyan)
           ("DONE"     . ,zenburn-green)
           ("CANCELED" . ,zenburn-red-1)))))

(with-eval-after-load 'org-agenda
  (defun nemacs-setup-org-agenda-mode ()
    "NEMACS Setup: Run this function in `org-agenda-mode-hook'."
    (hl-line-mode)
    (setq-local line-spacing 0.2))

  (defun nemacs-org-agenda-capture-related (&optional args)
    "Captures a new TODO related to the one at point.

NOTE: This is an special case of TODO task with `PROPERTIES'. The initial
property is: `RELATED'. Value of `RELATED' is the link to the task at point.
Task will be linked by `ID', using `org-store-link' and creating an unique `ID'
if the current task doesn't have one."
    (interactive "P")
    (call-interactively #'org-store-link)
    (setq-local org-capture-templates
                `(("L" "Linked" entry (file ,org-default-notes-file)
                  "* TODO %?
:PROPERTIES:
:RELATED: %a
:END:")))
    (org-capture :keys "L"))

  (defun nemacs-org-agenda-open-work-agenda ()
    "Opens the Work Agenda. Same as: `C-c a W'."
    (interactive)
    (org-agenda :keys "W"))

  (add-hook 'org-agenda-mode-hook #'nemacs-setup-org-agenda-mode)

  (define-key org-agenda-mode-map "T" #'nemacs-org-agenda-capture-related)
  (define-key org-agenda-mode-map "t" #'nemacs-org-capture-TODO)

  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c W") #'nemacs-org-agenda-open-work-agenda)

  (setq org-agenda-default-appointment-duration 60
        org-agenda-files `(,org-default-notes-file
                           ,(nemacs-org-file "todo.org"))
        org-agenda-start-on-weekday 0
        org-agenda-time-grid '((daily today required-time remove-match)
                               (700 800 900 1000 1100 1200
                                    1300 1400 1500 1600 1800 2000)
                               "......" "----------------"))

  (setq org-agenda-custom-commands
        `(("W" "Today @ work"
           ((agenda ""
                    ((org-agenda-compact-blocks t)
                     (org-agenda-remove-tags t)
                     (org-agenda-span 'day)))
            (tags-todo "#ROR"
                       ((org-agenda-overriding-header "Project: #ROR")
                        (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "#OK"
                       ((org-agenda-overriding-header "Project: #OK")
                        (org-agenda-todo-ignore-scheduled 'future)))))))

  (zenburn-with-color-variables
    (custom-set-faces
     `(org-agenda-date ((t (:foreground ,zenburn-fg-05))))
     `(org-agenda-date-today
       ((t (:bold t :foreground ,zenburn-yellow-2 :height 175 :italic nil))))
     `(org-agenda-date-weekend ((t (:bold t :foreground ,zenburn-fg-05))))
     `(org-time-grid ((t (:foreground ,zenburn-yellow)))))))

(with-eval-after-load 'org-bullets
  (setq org-bullets-bullet-list '("▲" "●" "■" "✶" "◉" "○" "○")
        org-bullets-face-name 'variable-pitch))

(with-eval-after-load 'org-capture
  (defun nemacs-org-capture-TODO ()
    "Captures a new `TODO'. Same as `C-c c T'"
    (interactive)
    (org-capture :keys "T"))

  (global-set-key (kbd "s-N") #'nemacs-org-capture-TODO)
  (global-set-key (kbd "C-c c") #'org-capture)

  (setq org-capture-templates
        `(("T" "Create a TODO task"
           entry (file ,org-default-notes-file)
           "* TODO %?"))))

(with-eval-after-load 'org-id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name ".org-id" nemacs-org-dir)))
