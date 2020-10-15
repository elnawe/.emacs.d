;;; setup.el --- NEMACS Setup file.

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

(defgroup NEMACS nil
  "NEMACS-related customization."
  :group 'development)

(defcustom nemacs-setup-complete nil
  "NEMACS setup variable. It changes when `(nemacs-start-setup)' runs."
  :type 'boolean
  :group 'NEMACS)

(defun nemacs-start-setup ()
  "Check if NEMACS setup has been done already (using `nemacs-setup-complete'),
if not, starts the setup process for the first time."
  (when (not (bound-and-true-p nemacs-setup-complete))
    (kill-buffer "*Messages*")
    (switch-to-buffer "*Messages*")
    (message "=====================================================")
    (message "It seems this is the first time you're running NEMACS.")
    (message "There's a setup that we need to go through in order")
    (message "to make NEMACS usable for you.")
    (message "")
    (message "NEMACS: Starting Setup... Press anything to continue.")

    (read-event)
    (customize-save-variable 'nemacs-setup-complete t)))

(nemacs-start-setup)

(defun nemacs-reset-setup ()
  (interactive)
  "Resets the value of `nemacs-setup-complete' and runs the NEMACS setup."
  (customize-save-variable 'nemacs-setup-complete nil)
  (nemacs-start-setup))
