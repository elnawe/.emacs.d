;;; core/utils/utils.el --- NEMACS Utils File.

;; This `utils' file provides the user with some functions that can be
;; used to improve the experience on configuring NEMACS.

;; Copyright (C) 2017 ~ 2021 Nahuel Jesús Sacchetti <me@nsacchetti.com>

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

(defvar nemacs-not-installed-packages-list '()
  "List of packages not installed that are required by NEMACS.

Items are added to this list if `nemacs-ensure-system-package'
cannot find the package in the system.")

(defvar nemacs-not-installed-error-list '()
  "List of packages that will throw an error if they are not installed.")

(defun nemacs-ensure-system-package (pkg &optional should-error)
  "Checks for the package `pkg' existance in the system.

Needs `exec-path' to be set in order to work.

If `should-error' is non-nil, adds `pkg' to
`nemacs-not-installed-error-list', which will error after NEMACS
finishes loading. By default, `pkg' will be added to
`nemacs-not-installed-packages-list' without erroring."
  (when (not (executable-find pkg))
    (add-to-list 'nemacs-not-installed-packages-list pkg)

    (when should-error
      (add-to-list 'nemacs-not-installed-error-list pkg))))

(defun nemacs-reset-not-installed-packages-list ()
  "Resets the `nemacs-not-installed-packages-list' to its initial
state."
  (interactive)
  (setq nemacs-not-installed-error-list '()
        nemacs-not-installed-packages-list '()))

(defun nemacs-ensure-system-check-errors ()
  "Checks for `nemacs-not-installed-error-list'. If the list is
not empty, will throw an error with the packages needed to
install to continue using NEMACS."
  (when nemacs-not-installed-error-list
    (error "The following required packages are missing: %s" nemacs-not-installed-error-list)))

(defun nemacs-install (pkg &rest body)
  "Checks for `pkg'. If it's not installed, install it."
  ;; Check for contents
  (when (not package-archive-contents)
    (package-refresh-contents))
  ;; Check for `pkg', install if it's not already installed.
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (debug)
    (package-install pkg))
  (debug)

  (unless (package-installed-p pkg)
    `(progn
       ;; Run the body after loading module
       (require pkg)

       (with-eval-after-load pkg
         ,@body
         (message "%s load complete" pkg)))))

(provide 'utils)
