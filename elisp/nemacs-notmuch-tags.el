;;; nemacs-notmuch-tags.el --- NEMACS Notmuch Tags.

;; Support library to configure the Notmuch tags in NEMACS. This is done by
;; defining keybindings to `notmuch-show' and `notmuch-search' to change, add or
;; remove tags.
;;
;; NOTE: This library should be loaded after requiring `notmuch'.

;; Copyright (C) 2017 ~ 2019 Nahuel Jesús Sacchetti <me@nsacchetti.com>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar nemacs-notmuch-tags '("calendar"
                              "chat"
                              "code"
                              "deleted"
                              "inbox"
                              "itx"
                              "paychex"
                              "spam")
  "Tags usually used in NEMACS Notmuch.")

(defun nemacs-notmuch-mark-as-spam ()
  "Mark message as spam."
  (interactive)
  (cond ((eq major-mode 'notmuch-search-mode)
         (notmuch-search-tag '("+spam" "-inbox")))
        ((eq major-mode 'notmuch-show-mode)
         (notmuch-show-tag '("+spam" "-inbox")))))

(defun nemacs-notmuch-toggle-inbox ()
  "Mark message as spam."
  (interactive)
  (cond ((eq major-mode 'notmuch-search-mode)
         (if (member "inbox" (notmuch-search-get-tags))
             (notmuch-search-tag '("-inbox"))
           (notmuch-search-tag '("+inbox"))))
        ((eq major-mode 'notmuch-show-mode)
         (if (member "inbox" (notmuch-show-get-tags))
             (notmuch-show-tag '("-inbox"))
           (notmuch-show-tag '("+inbox"))))))

(defun nemacs-notmuch-toggle-deleted ()
  "Mark message as spam."
  (interactive)
  (cond ((eq major-mode 'notmuch-search-mode)
         (if (member "deleted" (notmuch-search-get-tags))
             (notmuch-search-tag '("-deleted"))
           (notmuch-search-tag '("+deleted"))))
        ((eq major-mode 'notmuch-show-mode)
         (if (member "deleted" (notmuch-show-get-tags))
             (notmuch-show-tag '("-deleted"))
           (notmuch-show-tag '("+deleted"))))))

(defun nemacs-notmuch-toggle-tag-interactively ()
  "Add or removes a tag interactively with the help of `Helm'."
  (interactive)
  (let ((tag (helm :sources (helm-build-sync-source "Tags"
                                       :candidates nemacs-notmuch-tags
                                       :fuzzy-match t)
                            :buffer "*Notmuch Tags*")))
    (cond ((eq major-mode 'notmuch-search-mode)
           (if (member tag (notmuch-search-get-tags))
               (notmuch-search-tag `(,(concat "-" tag)))
             (notmuch-search-tag `(,(concat "+" tag)))))
          ((eq major-mode 'notmuch-show-mode)
           (if (member tag (notmuch-show-get-tags))
               (notmuch-show-tag `(,(concat "-" tag)))
             (notmuch-show-tag `(,(concat "+" tag))))))))

(provide 'nemacs-notmuch-tags)
