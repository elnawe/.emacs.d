;;; nemacs-org-ledger.el --- NEMACS Org and Ledger Support.

;; Utilities to facilitate access and usage of my modules. Used to share code
;; between the modules: `finances', `gtd' and `wm'.

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

(defvar nemacs-org-dir (concat nemacs-dropbox-dir "Notes/")
  "Default directory for all the Org files related to notes.")

(defvar nemacs-ledger-dir (concat nemacs-dropbox-dir "Finances")
  "Default directory for all the Ledger files related to finances.")

(defun nemacs-org-file (filename)
  "Expands from `nemacs-org-dir', appending `filename'."
  (expand-file-name filename nemacs-org-dir))

(defun nemacs-ledger-file (filename)
  "Expands from `nemacs-ledger-dir', appending `filename'."
  (expand-file-name filename nemacs-ledger-dir))

(defun nemacs-org-ledger-open ()
  "Prompts to open a file related to some NEMACS modules by just
pressing a key."
  (interactive)
  (let ((option (read-char "Open file:
[b]ooking\t\t [i]nbox\t\t [t]odo\t\t [q]uit")))
    (cond ((char-equal option (string-to-char "b"))
           (find-file (nemacs-ledger-file "booking.ledger")))
          ((char-equal option (string-to-char "i"))
           (find-file (nemacs-org-file "inbox.org")))
          ((char-equal option (string-to-char "t"))
           (find-file (nemacs-org-file "todo.org")))
          (t
           (keyboard-quit)))))

(provide 'nemacs-org-ledger)
