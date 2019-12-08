;; TODO: New calendar entry is only for PERSONAL calendar. Need to create
;; a function to make it so I can add new entries to other calendars.
;; TODO: Add new capture template for tech specs

(require 'idle-org-agenda)
(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-capture)
(require 'org-id)
(require 'org-super-agenda)

(with-eval-after-load 'idle-org-agenda
  (setq idle-org-agenda-interval 3000
        idle-org-agenda-key "d")

  (idle-org-agenda-mode))

(with-eval-after-load 'org
  (require 'nemacs-org-ledger)

  (defun nemacs-org-journal-capture-weekly ()
    "Captures a weekly entry for the Journal. Same as `C-c c J w'."
    (interactive)
    (org-capture :keys "Jw"))

  (defun nemacs-org-journal-capture-monthly ()
    "Captures a monthly entry for the Journal. Same as `C-c c J m'."
    (interactive)
    (org-capture :keys "Jm"))

  (defun nemacs-setup-org-mode ()
    "NEMACS Setup: Run this function in `org-mode-hook'"
    (flyspell-mode)
    (org-bullets-mode)
    (org-indent-mode)
    (turn-on-visual-line-mode)
    (setq-local line-spacing 0.1))

  (add-hook 'org-mode-hook #'nemacs-setup-org-mode)
  (add-hook 'org-after-refile-insert-hook #'org-save-all-org-buffers)
  (add-hook 'org-archive-hook #'org-save-all-org-buffers)

  (global-set-key (kbd "C-c l") #'org-store-link)

  (setq org-archive-location (nemacs-org-file "archive.org::datetree/")
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-deadline-warning-days 7
        org-default-notes-file (nemacs-org-file "inbox.org")
        org-descriptive-links t
        org-directory nemacs-org-dir
        org-ellipsis " […]"
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-return-follows-link t
        org-startup-folded nil
        org-startup-truncated nil
        org-support-shift-select 'always
        org-tags-column -75)

  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets `(((,org-default-notes-file
                               ,(nemacs-org-file "maybe.org")
                               ,(nemacs-org-file "todo.org")) :level . 1))
        org-refile-use-outline-path 'file)

  (setq org-tag-persistent-alist
        '(;; Context
          ("@computer" . ?c)
          ("@emacs"    . ?e)
          ("@gameroom" . ?g)
          ("@home"     . ?h)
          ("@meeting"  . ?m)
          ("@office"   . ?o)
          ("@wiki"     . ?w)

          ;; Projects
          ("Personal"         . ?P)
          ("CompanyDirectory" . ?C)
          ("Retirement"       . ?R)

          ;; Teams
          ("OK"  . ?x)
          ("ROR" . ?z)))

 (setq org-todo-keywords
       '((sequence "TODO(t!)"
                   "CLEARED(c@)"
                   "PROJECT(p@)"
                   "WAITING(w@)"
                   "|"
                   "DONE(D@)"
                   "CANCELED(C@)")))

 (with-c64-color-variables
   (setq org-todo-keyword-faces
         `(("TODO"     . ,c64-8)
           ("CLEARED"  . ,c64-7)
           ("PROJECT"  . ,c64-4)
           ("WAITING"  . ,c64-3)
           ("DONE"     . ,c64-5)
           ("CANCELED" . ,c64-2)

           ;; Special states
           ("JOURNAL"  . ,c64-10)))))

(with-eval-after-load 'org-agenda
  (defun nemacs-setup-org-agenda-mode ()
    "NEMACS Setup: Run this function in `org-agenda-mode-hook'."
    (hl-line-mode)
    (setq-local line-spacing 0.2))

  (defun nemacs-org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defun nemacs-org-agenda-capture-related (&optional args)
    "Captures a new TODO related to the one at point.

NOTE: This is an special case of TODO task with `PROPERTIES'. The initial
property is: `RELATED'. Value of `RELATED' is the link to the task at point.
Task will be linked by `ID', using `org-store-link' and creating an unique `ID'
if the current task doesn't have one."
    (interactive "P")
    (let ((org-capture-templates
           `(("L" "Linked" entry (file ,org-default-notes-file)
              "* TODO %?
:PROPERTIES:
:RELATED: %a
:END:"))))
      (call-interactively #'org-store-link)
      (org-capture :keys "L")))

  (defun nemacs-org-agenda-open-default-agenda ()
    "Opens the default NEMACS Agenda."
    (interactive)
    (org-agenda :keys "d"))

  (add-hook 'org-agenda-mode-hook #'nemacs-setup-org-agenda-mode)

  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh `org-agenda' with `nemacs-org-agenda-refresh'."
    (nemacs-org-agenda-refresh))

  (define-key org-agenda-mode-map "T" #'nemacs-org-agenda-capture-related)
  (define-key org-agenda-mode-map "t" #'nemacs-org-capture-TODO)

  (global-set-key (kbd "C-c a") #'nemacs-org-agenda-open-default-agenda)

  (setq org-agenda-category-icon-alist
        '(("BIRTHDAY" "~/.emacs.d/icons/org/birthday.png" nil nil :ascent center)
          ("HOLIDAY" "~/.emacs.d/icons/org/holiday.png" nil nil :ascent center)
          ("INBOX" "~/.emacs.d/icons/org/inbox.png" nil nil :ascent center)
          ("ITX" "~/.emacs.d/icons/org/itx.png" nil nil :ascent center)
          ("PERSONAL" "~/.emacs.d/icons/org/personal.png" nil nil :ascent center)
          ("PROJECT" "~/.emacs.d/icons/org/project.png" nil nil :ascent center)
          ("TODO" "~/.emacs.d/icons/org/work.png" nil nil :ascent center)
          (".*" '(space . (:width (16))))))

  (setq org-agenda-default-appointment-duration 60
        org-agenda-files `(,org-default-notes-file
                           ,(nemacs-org-file "todo.org")
                           ,(nemacs-org-file "projects.org")
                           ,(nemacs-org-file "calendar.org"))
        org-agenda-start-on-weekday 0
        org-agenda-time-grid '((daily today required-time remove-match)
                               (600 800 1000 1200 1400 1600)
                               "......" "----------------"))


  ;; TODO: I don't need org-recur anymore but might use something similar
  ;; to mark tasks as done.
  ;; (define-key org-recur-mode-map (kbd "C-c d") #'org-recur-finish)
  ;; (define-key org-recur-agenda-mode-map "d" #'org-recur-finish)
  ;; Keeping these because they are interesting settings.
  (setq org-log-done 'time
        org-log-redeadline nil
        org-log-reschedule nil
        org-read-date-prefer-future 'time)

  (setq org-agenda-custom-commands
        `(("d" "NEMACS Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Calendar" :time-grid t)))))
            (todo "" ((org-agenda-overriding-header "Stuck Projects")
                      (org-super-agenda-groups
                       '((:name none :children t
                                :discard (:anything t))))))
            (todo "" ((org-agenda-overriding-header "Company Directory")
                      (org-agenda-hide-tags-regexp "CompanyDirectory")
                      (org-super-agenda-groups
                       '((:name none
                                :and (:tag "CompanyDirectory" :scheduled nil
                                           :not (:todo "PROJECT")))
                         (:discard (:anything t))))))
            (todo "" ((org-agenda-overriding-header "Retirement")
                      (org-agenda-hide-tags-regexp "Retirement")
                      (org-super-agenda-groups
                       '((:name none
                                :and (:tag "Retirement" :scheduled nil))
                         (:discard (:anything t))))))
            (todo "" ((org-agenda-overriding-header "Inbox")
                      (org-agenda-files `(,org-default-notes-file)))))))))

(with-eval-after-load 'org-bullets
  (setq org-bullets-bullet-list '("▲" "●" "■" "✶" "◉" "○" "○")
        org-bullets-face-name 'org-bullets))

(with-eval-after-load 'org-capture
  (defvar nemacs-org-calendar-capture-template
    "* %(nemacs-org-prompt-for-recurrence) %^{Event summary}
%^{Event date}t
%?")

  (defvar nemacs-org-calendar-itx-capture-template
    "* %(nemacs-org-prompt-for-recurrence) %^{Event summary} %?
%^{Event date}t %^{ORGANIZER}p %^{LOCATION}p")

  (defun nemacs-org-prompt-for-recurrence ()
    (let ((recurrence (read-string "Event recurrence:
- +2: Every other day.
- +w: Every week.
- Thu: Every Thursday.
- Sun,Sat: Every Sunday and Saturday.
- Wkdy: Every weekday.
- <day of the month> (i.e. 1): That day of every month.
- <Empty> will not add recurrence.

Enter Recurrence and press RET: ")))
      (if (not (string-empty-p recurrence))
          (concat "|" recurrence "|")
        "")))

  (defun nemacs-org-capture-TODO ()
    "Captures a new `TODO'. Same as `C-c c T'"
    (interactive)
    (org-capture :keys "T"))

  (global-set-key (kbd "C-c c") #'org-capture)

  (setq org-capture-templates
        `(("T" "Create a TODO task"
           entry (file ,org-default-notes-file)
           "* TODO %?")
          ("C" "New Calendar entry in")
          ("Cp" "The personal calendar"
           entry (file+headline ,(nemacs-org-file "calendar.org") "PERSONAL")
           ,nemacs-org-calendar-capture-template)
          ("Ci" "The ITX calendar"
           entry (file+headline ,(nemacs-org-file "calendar.org") "ITX")
           ,nemacs-org-calendar-itx-capture-template)
          ("J" "New Journal entry with type")
          ("Jw" "Weekly"
           entry (file+datetree ,(nemacs-org-file "journal.org"))
           (file ,(nemacs-org-template "weekly.org"))
           :immediate-finish t :jump-to-captured t)
          ("Jm" "Monthly"
           entry (file+datetree ,(nemacs-org-file "journal.org"))
           (file ,(nemacs-org-template "monthly.org"))
           :immediate-finish t :jump-to-captured t))))

(with-eval-after-load 'org-id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name ".org-id" nemacs-org-dir)))

(with-eval-after-load 'org-super-agenda
  (org-super-agenda-mode))
