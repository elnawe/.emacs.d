;;; nemacs-notmuch-unread.el --- NEMACS Notmuch Unread.

;; Minor mode: `notmuch-unread-mode'. This minor mode adds the number of unread
;; e-mail to the modeline.

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

(defvar nemacs-notmuch-unread-mode-line-string nil
  "String to display in the mode line.")

(defvar nemacs-notmuch-unread-update-timer nil
  "Timer for updating the mode line.")

(defcustom nemacs-notmuch-unread-update-interval 15
  "The number of seconds to wait in between updates."
  :type 'integer
  :group 'nemacs-notmuch-unread)

(defcustom nemacs-notmuch-unread-search-term "tag:unread"
  "The search term to pass to notmuch count."
  :type 'string
  :group 'nemacs-notmuch-unread)

(defun nemacs-notmuch-unread-count ()
  "Return the number of messages that match
`nemacs-notmuch-unread-search-term`."
  (string-to-number
   (replace-regexp-in-string
    "\n" ""
    (notmuch-command-to-string "count" nemacs-notmuch-unread-search-term))))

(defun nemacs-notmuch-unread-update-handler ()
  "Update the mode line."
  (setq nemacs-notmuch-unread-mode-line-string
        (format " M:%d"
                (nemacs-notmuch-unread-count)))
  (force-mode-line-update))

(define-minor-mode nemacs-notmuch-unread-mode
  "Display unread mail count in the mode line"
  :global t
  :require 'notmuch
  (and nemacs-notmuch-unread-update-timer
       (cancel-timer nemacs-notmuch-unread-update-timer))
  (if nemacs-notmuch-unread-mode
      (progn
        (add-to-list 'global-mode-string
                     'nemacs-notmuch-unread-mode-line-string t)
        (setq nemacs-notmuch-unread-update-timer
              (run-at-time nil nemacs-notmuch-unread-update-interval
                           'nemacs-notmuch-unread-update-handler)))
    (setq global-mode-string
          (delq 'nemacs-notmuch-unread-mode-line-string
                global-mode-string))))

(provide 'nemacs-notmuch-unread)
