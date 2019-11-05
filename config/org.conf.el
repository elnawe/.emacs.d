(use-package org
  :if (eq system-type 'gnu/linux)
  :ensure org-plus-contrib
  :custom
  (org-archive-location "~/Dropbox/orgs/archive.org::datetree/")
  (org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
  (org-deadline-warning-days 7)
  (org-default-notes-file "~/Dropbox/orgs/inbox.org")
  (org-descriptive-links t)
  (org-directory "~/Dropbox/orgs/")
  (org-ellipsis "\u21b4")
  (org-email-link-description-format "Email %c: [%d] %.15s")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-support-shift-always 'always)
  (org-tags-column -100)
  (org-todo-keywords '((sequence "TODO(t!)"
                                 "NEXT(n@)"
                                 "STARTED"
                                 "PROJECT"
                                 "|"
                                 "DONE(d)"
                                 "CANCELED(c@)"))))

(use-package org-agenda
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-org-agenda-hook ()
    (hl-line-mode)
    (setq line-spacing 0.2))

  (defun nemacs-org-agenda-startup ()
    (interactive)
    (org-agenda :keys "gtd"))

  (defun nemacs-org-agenda-mark-as-done (&optional arg)
    (interactive "P")
    (org-agenda-todo "DONE"))

  (defun nemacs-org-agenda-mark-as-done-capture-follow-up (&optional arg)
    (interactive "P")
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))
  :hook (org-agenda-mode . nemacs-org-agenda-hook)
  :bind (("C-c a" . org-agenda)
         ("C-c d" . nemacs-org-agenda-startup)
         (:map org-agenda-mode-map
               ("g" . org-gcal-fetch)
               ("x" . nemacs-org-agenda-mark-as-done)
               ("X" . nemacs-org-agenda-mark-as-done-capture-follow-up)))
  :custom
  (org-agenda-category-icon-alist '(("Birthday" "~/.emacs.d/icons/org/birthday.png" nil nil :ascent center)
                                    ("Calendar" "~/.emacs.d/icons/org/calendar.png" nil nil :ascent center)
                                    ("Contacts" "~/.emacs.d/icons/org/contacts.png" nil nil :ascent center)
                                    ("Emacs" "~/.emacs.d/icons/org/emacs.png" nil nil :ascent center)
                                    ("Holiday" "~/.emacs.d/icons/org/holiday.png" nil nil :ascent center)
                                    ("Inbox" "~/.emacs.d/icons/org/inbox.png" nil nil :ascent center)
                                    ("Life" "~/.emacs.d/icons/org/qol.png" nil nil :ascent center)
                                    ("Personal" "~/.emacs.d/icons/org/personal.png" nil nil :ascent center)
                                    ("Someday" "~/.emacs.d/icons/org/someday.png" nil nil :ascent center)
                                    ("Work" "~/.emacs.d/icons/org/itx.png" nil nil :ascent center)
                                    (".*" '(space . (:width (16))))))
  (org-agenda-custom-commands '(("r" "Daily Review" ((tags-todo "Today")
                                                     (tags-todo "Tomorrow")
                                                     (agenda "" ((org-agenda-span 2)
                                                                 (org-deadline-warning-days 7)
                                                                 (org-agenda-start-on-weekday nil)))))
                                ("u" "Unscheduled TODOs" ((todo "TODO"
                                                                ((org-agenda-overriding-header "Unscheduled TODO")
                                                                 (org-agenda-todo-ignore-scheduled 'future)))))
                                ("gtd" "My GTD Agenda" ((agenda "" ((org-agenda-overriding-header "Getting Things Done")
                                                                    (org-agenda-span 1)
                                                                    (org-deadline-warning-days 7)
                                                                    (org-agenda-start-on-weekday nil)))))))
  (org-agenda-files '("~/Dropbox/orgs/inbox.org"
                      "~/Dropbox/orgs/calendar/ITX Calendar.org"
                      "~/Dropbox/orgs/calendar/Personal Calendar.org"))
  (org-agenda-inhibit-startup nil)
  (org-agenda-show-future-repeats nil)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-start-on-weekday 0)
  (org-agenda-time-grid '((daily today required-time remove-match)
                          (700 800 900 1000 1100 1200 1300 1400 1500 1600 1800 2000)
                          "......" "----------------"))
  :custom-face
  (org-agenda-calendar-event ((t (:foreground "NavyBlue"))))
  (org-agenda-date ((t (:foreground "gray19"))))
  (org-agenda-date-today ((t (:inherit org-agenda-date
                                       :underline t))))
  (org-agenda-date-weekend ((t (:foreground "gray30"))))
  (org-agenda-structure ((t (:foreground "gray19"
                                         :weight bold))))

  (org-scheduled ((t (:foreground "blue"))))
  (org-scheduled-today ((t (:foreground "blue3"))))
  (org-time-grid ((t (:foreground "gray50")))))

(use-package org-bullets
  :if (eq system-type 'gnu/linux)
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "▲" "■" "✶" "◉" "○" "○")))

(use-package org-capture
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :after org
  :preface
  (defvar nemacs-org-capture-quick-template "* TODO %^{Task}")

  (defvar nemacs-org-capture-basic-template "* TODO %?")

  (defun nemacs-org-capture-add-basic-properties ()
    "Adds basic properties to the captured stuff from org-capture. Runs on `org-capture-finalize-hook'."
    (interactive)
    (org-set-property "CAPTURED" (format-time-string "[%Y-%m-%d %H:%M]"))
    (org-id-get-create))

  (defun nemacs-org-capture-quick-task ()
    "Captures a quick task. Used on `M-m' and across the operating system."
    (interactive)
    (org-capture :keys "q"))
  :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
  :bind (("M-m"   . nemacs-org-capture-quick-task)
         ("C-c c" . org-capture))
  :custom
  (org-capture-templates `(("t" "Add a TODO task" entry (file ,org-default-notes-file)
                            ,nemacs-org-capture-basic-template
                            :empty-lines 1)
                           ("q" "Add Quick TODO task" entry (file ,org-default-notes-file)
                            ,nemacs-org-capture-quick-template
                            :empty-lines 1 :immediate-finish t))))

(use-package org-clock
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :after org
  :custom
  (org-clock-in-resume t)
  (org-clock-in-switch-to-state "STARTED")
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-when-down t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil))

(use-package org-contacts
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :after org
  :custom
  (org-contacts-birthday-format " %l")
  (org-contacts-files '("~/Dropbox/orgs/contacts.org")))

(use-package org-id
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :after org
  :custom
  (org-id-locations-file "~/Dropbox/orgs/config/org-ids")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org-gcal
  :if (eq system-type 'gnu/linux)
  :after org
  :preface
  (defvar nemacs-org-gcal-auth (auth-source-user-and-password "org-gcal"))
  :custom
  (org-gcal-client-id (car nemacs-org-gcal-auth))
  (org-gcal-client-secret (cadr nemacs-org-gcal-auth))
  (org-gcal-file-alist '(("nahueljsacchetti@gmail.com" . "~/Dropbox/orgs/calendar/Personal Calendar.org")
                         ("lprcql81oieu0v9kb3kf1utcgg@group.calendar.google.com" . "~/Dropbox/orgs/calendar/ITX Calendar.org")))
  :init
  (setq org-gcal-dir (concat nemacs-cache-dir "org-gcal/")))


;; (use-package org
;;   :ensure org-plus-contrib
;;   :preface
;;   (defun nemacs-open-org-inbox-file ()
;;     (interactive)
;;     (find-file org-default-notes-file))

;;   (defun nemacs-open-org-todo-file ()
;;     (interactive)
;;     (find-file "~/Dropbox/orgfiles/todo.org"))

;;   (defun nemacs-org-mode-hook ()
;;     (setq line-spacing 0.2)
;;     (flyspell-mode 1)
;;     (org-bullets-mode 1)
;;     (turn-on-auto-fill))
;;   :hook ((org-after-refile-insert . save-buffer)
;;          (org-mode                . nemacs-org-mode-hook))
;;   :bind (("C-c i" . nemacs-open-org-inbox-file)
;;          ("C-c t" . nemacs-open-org-todo-file)
;;          ("C-c l" . org-store-link))
;;   :custom
;;   (diary-file "~/Dropbox/orgfiles/calendar/Org Calendar.org")
;;   (org-edit-src-content-indentation 0)
;;   (org-edit-src-persistent-message nil)


;;   (org-outline-path-complete-in-steps nil)
;;   (org-refile-allow-creating-parent-nodes 'confirm)
;;   (org-refile-targets `(((,org-default-notes-file "~/Dropbox/orgfiles/someday.org" "~/Dropbox/orgfiles/todo.org") :maxlevel . 3)))
;;   (org-refile-use-outline-path t)
;;   (org-src-fontify-natively t)
;;   (org-src-preserve-indentation t)
;;   (org-src-tab-acts-natively t)
;;   (org-src-window-setup 'current-window)

;;   (org-tag-persistent-alist '(("@errand" . ?e)
;;                               ("@home"   . ?h)
;;                               ("@office" . ?o)))
;;   (org-todo-keywords '((sequence "TODO(t!)"
;;                                  "NEXT(n@)"
;;                                  "STARTED"
;;                                  "PROJECT"
;;                                  "|"
;;                                  "DONE(d)"
;;                                  "CANCELED(c@)")))
;;   (org-todo-keyword-faces '(("TODO"     . org-todo)
;;                             ("PROJECT"  . (:foreground "orange"
;;                                                        :weight bold))
;;                             ("STARTED"  . (:foreground "LimeGreen"
;;                                                        :weight bold))
;;                             ("NEXT"     . (:foreground "blue"
;;                                                        :weight bold))
;;                             ("DONE"     . org-done)
;;                             ("CANCELED" . (:foreground "red"
;;                                                        :underline t
;;                                                        :weight bold))))

;; (use-package org-capture
;;   :ensure nil
;;   :after org
;;   :preface
;;   (defvar nemacs-org-capture-basic-template "* TODO %^{Task}
;;   :PROPERTIES:
;;   :CAPTURED: %<%Y-%m-%d %H:%M>
;;   :END:")
;;   (defvar nemacs-org-capture-link-template "* TODO %^{Task} :#LINK:
;;   :PROPERTIES:
;;   :CAPTURED: %<%Y-%m-%d %H:%M>
;;   :LINK:     %a
;;   :END:")
;;   (defvar nemacs-org-capture-contact-template "* %(org-contacts-template-name)
;;   :PROPERTIES:
;;   :ADDRESS:  %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
;;   :BIRTHDAY: %^{YYYY-MM-DD}
;;   :EMAIL:    %(org-contacts-template-email)
;;   :PHONE:    %^{+5493411234567}
;;   :NOTE:     %^{Note}
;;   :END:")

;;   (defun nemacs-org-capture (&optional args)
;;     (interactive "P")
;;     (if current-prefix-arg
;;         (org-capture :keys "T")
;;       (org-capture :keys "t")))

;;   (defun nemacs-org-capture-add-basic-properties ()
;;     (interactive)
;;     (org-id-get-create))

;;   (defun nemacs-org-capture-review-daily ()
;;     (interactive)
;;     (progn
;;       (org-capture nil "rd")
;;       (org-capture-finalize t)
;;       (org-speed-move-safe 'outline-up-heading)
;;       (org-narrow-to-subtree)
;;       (org-gcal-fetch)
;;       (org-clock-in)))

;;   (defun nemacs-org-capture-review-weekly ()
;;     (interactive)
;;     (progn
;;       (org-capture nil "rw")
;;       (org-capture-finalize t)
;;       (org-speed-move-safe 'outline-up-heading)
;;       (org-narrow-to-subtree)
;;       (org-gcal-fetch)
;;       (org-clock-in)))
;;   :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
;;   :bind (("M-m"     . nemacs-org-capture)
;;          ("C-c c"   . org-capture)
;;          ("C-c r d" . nemacs-org-capture-review-daily)
;;          ("C-c r w" . nemacs-org-capture-review-weekly))
;;   :custom
;;   (org-capture-templates `(("t" "Add TODO Task" entry (file ,org-default-notes-file)
;;                             ,nemacs-org-capture-basic-template
;;                             :empty-lines 1 :immediate-finish t)
;;                            ("T" "Add Linked TODO Task" entry (file ,org-default-notes-file)
;;                             ,nemacs-org-capture-link-template
;;                             :empty-lines 1 :immediate-finish t)
;;                            ("c" "Add Contact" entry (file "~/Dropbox/orgfiles/contacts.org")
;;                             ,nemacs-org-capture-contact-template
;;                             :empty-lines 1)

;;                            ("rd" "Review: Daily" entry (file+olp+datetree "/tmp/reviews.org")
;;                             (file "~/Dropbox/orgfiles/templates/daily-review.template.org"))
;;                            ("rw" "Review: Weekly" entry (file+olp+datetree "/tmp/reviews.org")
;;                             (file "~/Dropbox/orgfiles/templates/weekly-review.template.org")))))
