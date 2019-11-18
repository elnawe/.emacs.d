(use-package org
  :ensure org-plus-contrib
  :preface
  (defun nemacs-org-mode-hook ()
    (interactive)
    (setq line-spacing 0.3)
    (flyspell-mode 1)
    (org-bullets-mode 1)
    (turn-on-auto-fill))

  (defun nemacs-org-open-inbox ()
    (interactive)
    (find-file org-default-notes-file))

  (defun nemacs-org-open-orgs ()
    (interactive)
    (cd "~/Dropbox/orgs/")
    (helm-find-files nil))

  (defun nemacs-org-sort-entries-by-todo ()
    ;; TODO: Make this workable
    "Sort the entries in the file by `TODO' keyword."
    (interactive)
    (save-excursion
      (outline-show-children 2)
      (re-search-backward (file-relative-name (buffer-file-name)))
      (org-sort-entries nil ?o)))
  :hook ((org-after-refile-insert     . org-save-all-org-buffers)
         (org-mode                    . nemacs-org-mode-hook))
  :bind (("C-c i" . nemacs-org-open-inbox)
         ("C-c t" . nemacs-org-open-orgs))
  :custom
  (org-archive-location "~/Dropbox/orgs/archive.org::datetree/")
  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))
  (org-deadline-warning-days 7)
  (org-default-notes-file "~/Dropbox/orgs/inbox.org")
  (org-descriptive-links t)
  (org-directory "~/Dropbox/orgs/")
  (org-ellipsis " \u21b4")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets `(((,org-default-notes-file
                          "~/Dropbox/orgs/someday.org"
                          "~/Dropbox/orgs/personal.org"
                          "~/Dropbox/orgs/work.org") :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-startup-folded t)
  (org-startup-truncated nil)
  (org-support-shift-always 'always)
  (org-tag-persistent-alist '(("#FALKOR"        . ?F)
                              ("#OK"            . ?O)
                              ("#ROR"           . ?R)
                              ("Need_Bug"       . ?B)
                              ("Linked"         . ?l)
                              ("Possible_Bug"   . ?b)
                              ("Possible_Story" . ?s)
                              ("Need_Story"     . ?S)
                              ("@computer"      . ?c)
                              ("@game_room"     . ?g)
                              ("@home"          . ?h)
                              ("@meeting"       . ?m)
                              ("@office"        . ?o)
                              ("@triage"        . ?t)))
  (org-tags-column -100)
  (org-todo-keywords '((sequence "TODO(t!)"
                                 "NEXT(n@)"
                                 "PROJECT(p)"
                                 "WAITING(w@)"
                                 "|"
                                 "DONE(d@)"
                                 "CANCELED(c@)")))
  (org-todo-keyword-faces '(("TODO"     . "#5D54E1")
                            ("NEXT"     . "#FEAC71")
                            ("PROJECT"  . "#F1FA8C")
                            ("WAITING"  . "#61BFFF")
                            ("DONE"     . "#50FA7B")
                            ("CANCELED" . "#FF5555"))))

(use-package org-agenda
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
               ("x" . nemacs-org-agenda-mark-as-done)
               ("X" . nemacs-org-agenda-mark-as-done-capture-follow-up)))
  :custom
  (org-agenda-category-icon-alist '(("Birthday"  "~/.emacs.d/icons/org/birthday.png" nil nil :ascent center)
                                    ("Calendar"  "~/.emacs.d/icons/org/calendar.png" nil nil :ascent center)
                                    ("Contacts"  "~/.emacs.d/icons/org/contacts.png" nil nil :ascent center)
                                    ("Holiday"   "~/.emacs.d/icons/org/holiday.png"  nil nil :ascent center)
                                    ("Life"      "~/.emacs.d/icons/org/qol.png"      nil nil :ascent center)
                                    ;; Using right now below this point.
                                    ("Personal"  "~/.emacs.d/icons/org/personal.png" nil nil :ascent center)
                                    ("Emacs"     "~/.emacs.d/icons/org/emacs.png"    nil nil :ascent center)
                                    ("Game Room" "~/.emacs.d/icons/org/gameroom.png" nil nil :ascent center)
                                    ("Inbox"     "~/.emacs.d/icons/org/inbox.png"    nil nil :ascent center)
                                    ("Home"      "~/.emacs.d/icons/org/home.png"     nil nil :ascent center)
                                    ("Someday"   "~/.emacs.d/icons/org/someday.png"  nil nil :ascent center)
                                    ("ITX"       "~/.emacs.d/icons/org/itx.png"      nil nil :ascent center)
                                    ("ROR"       "~/.emacs.d/icons/org/ror.png"      nil nil :ascent center)
                                    ("OK"        "~/.emacs.d/icons/org/ok.png"       nil nil :ascent center)
                                    ("FALKOR"    "~/.emacs.d/icons/org/falkor.png"   nil nil :ascent center)
                                    (".*" '(space . (:width (16))))))
  (org-agenda-custom-commands '(("u" "Unscheduled TODOs" ((todo "TODO"
                                                                ((org-agenda-overriding-header "Unscheduled TODO")
                                                                 (org-agenda-todo-ignore-scheduled 'future)))))))
  (org-agenda-files '("~/Dropbox/orgs/calendar.org"
                      "~/Dropbox/orgs/inbox.org"
                      "~/Dropbox/orgs/personal.org"
                      "~/Dropbox/orgs/work.org"))
  (org-agenda-inhibit-startup nil)
  (org-agenda-show-future-repeats nil)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-start-on-weekday 0)
  (org-agenda-time-grid '((daily today required-time remove-match)
                          (700 800 900 1000 1100 1200 1300 1400 1500 1600 1800 2000)
                          "......" "----------------")))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "▲" "■" "✶" "◉" "○" "○")))

(use-package org-capture
  :ensure nil
  :after org
  :preface
  (defvar nemacs-org-capture-basic-template "* TODO %?
  :PROPERTIES:
  :CAPTURED: <%<%Y-%m-%d %H:%M>>
  :END:")

  (defvar nemacs-org-capture-link-template "* TODO %? :Linked:
  :PROPERTIES:
  :CAPTURED: <%<%Y-%m-%d %H:%M>>
  :LINK:     %a
  :END:")

  (defvar nemacs-org-capture-contact-template "* %(org-contacts-template-name)
  :PROPERTIES:
  :ADDRESS:  -
  :BIRTHDAY: %^{YYYY-MM-DD}
  :EMAIL:    %(org-contacts-template-email)
  :PHONE:    -
  :NOTE:     -
  :END:")

  (defun nemacs-org-capture-add-basic-properties ()
    "Adds basic properties to the captured stuff from org-capture. Runs on `org-capture-finalize-hook'."
    (interactive)
    (org-id-get-create))

  (defun nemacs-org-capture-quick-task ()
    "Captures a quick task. Used on `M-n' and across the operating system (with `Super-N')."
    (interactive)
    (org-capture :keys "t"))

  (defun nemacs-org-capture-linked-task ()
    "Captures a linked quick task. Used on `M-N' and across the operating system (with `Super-N')."
    (interactive)
    (org-capture :keys "l"))

  ;; TODO: Make this function work to create a new Tech Spec in the right place with the right title.
  (defun nemacs-org-create-new-tech-spec (prompt)
    "Creates a new Technical Spec from the `tech-spec.template'"
    (interactive "^P"))
  :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
  :bind (("M-n"   . nemacs-org-capture-quick-task)
         ("M-N"   . nemacs-org-capture-linked-task)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  (org-capture-templates `(("t" "Add a TODO task" entry (file+headline ,org-default-notes-file "inbox.org")
                            ,nemacs-org-capture-basic-template)
                           ("l" "Add a Linked TODO task" entry (file+headline ,org-default-notes-file "inbox.org")
                            ,nemacs-org-capture-link-template))))

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-locations-file "~/Dropbox/orgs/org-ids")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package ox-confluence
  :ensure nil
  :after org-plus-contrib)
