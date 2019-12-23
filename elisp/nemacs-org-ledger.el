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

(defface nemacs-org-ledger-title '()
  "Custom face for the `nemacs-org-ledger-open' title."
  :group 'org-mode)

(defvar nemacs-org-dir (concat nemacs-dropbox-dir "Notes/")
  "Default directory for all the Org files related to notes.")

(defvar nemacs-org-template-dir (concat nemacs-dropbox-dir "org/templates/")
  "Directory where all the `org-mode' templates are saved.")

(defvar nemacs-ledger-dir (concat nemacs-dropbox-dir "Finances")
  "Default directory for all the Ledger files related to finances.")

(defun nemacs-org-file (filename)
  "Expands from `nemacs-org-dir', appending `filename'."
  (expand-file-name filename nemacs-org-dir))

(defun nemacs-org-template (filename)
  "Expands from `nemacs-org-template-dir', appending `filename'."
  (expand-file-name filename nemacs-org-template-dir))

(defun nemacs-ledger-file (filename)
  "Expands from `nemacs-ledger-dir', appending `filename'."
  (expand-file-name filename nemacs-ledger-dir))

(defun nemacs-org-ledger-formatted-string (string &optional separator)
  "Uses `string' to run a propertize and return the right value.
When `separator' is non-nill, will add the character `|' to the result.

For example: Given `string' is inbox, return `[i]nbox'."
  (let ((entry-key
         (propertize
          (concat "[" (substring string 0 1) "]")
          'face 'bold))
        (rest-string (substring string 1)))
    (concat entry-key
            rest-string
            (when separator " | "))))

(defun nemacs-org-ledger-open ()
  "Prompts to open a file related to some NEMACS modules by just
pressing a key."
  (interactive)
  (let ((option
         (read-char
          (concat
           (propertize  "Open file:\n"
                        'face 'nemacs-org-ledger-title)
           (nemacs-org-ledger-formatted-string "booking" t)
           (nemacs-org-ledger-formatted-string "calendar" t)
           (nemacs-org-ledger-formatted-string "inbox" t)
           (nemacs-org-ledger-formatted-string "journal" t)
           (nemacs-org-ledger-formatted-string "todo" t)
           (nemacs-org-ledger-formatted-string "quit")))))
    (cond ((char-equal option (string-to-char "b"))
           (find-file (nemacs-ledger-file "booking.ledger")))
          ((char-equal option (string-to-char "c"))
           (find-file (nemacs-org-file "calendar.org")))
          ((char-equal option (string-to-char "i"))
           (find-file (nemacs-org-file "inbox.org")))
          ((char-equal option (string-to-char "j"))
           (find-file (nemacs-org-file "journal.org")))
          ((char-equal option (string-to-char "p"))
           (find-file (nemacs-org-file "projects.org")))
          ((char-equal option (string-to-char "t"))
           (find-file (nemacs-org-file "todo.org")))
          (t
           (keyboard-quit)))))

(with-vb-color-variables
  (custom-set-faces
   `(nemacs-org-ledger-title
     ((t (:background ,vb-background :bold t
                      :box (:color ,vb-background :line-width 6)
                      :foreground unspecified :height 200))))))

(provide 'nemacs-org-ledger)
