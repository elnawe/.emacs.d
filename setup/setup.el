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

(defvar nemacs-settings-file (expand-file-name "nemacs-settings.el" nemacs-etc-dir)
  "Location of the `nemacs-settings.el' file. Generated by the NEMACS Setup")

(defun nemacs-start-setup ()
  "Check if NEMACS setup has been done already (using `nemacs-setup-complete'),
if not, starts the setup process for the first time."
  (when (file-exists-p nemacs-settings-file)
    (load nemacs-settings-file))
  (when (or (not (bound-and-true-p nemacs-setup-complete))
            (bound-and-true-p nemacs-reset-setup-force))
    (progn
      (kill-buffer "*Messages*")
      (switch-to-buffer "*Messages*")
      (message "It seems this is the first time you're running NEMACS.")
      (message "There's a setup that we need to go through in order")
      (message "to make NEMACS usable for you.\n")
      (message "NEMACS: Starting Setup... Press anything to continue.")

      (read-event)

      (setq nemacs-module-email-enabled (y-or-n-p "Enable EMAIL module? "))
      (setq nemacs-module-finance-enabled (y-or-n-p "Enable FINANCE module? "))
      (setq nemacs-module-gtd-enabled (y-or-n-p "Enable GTD module? "))
      (setq nemacs-module-navigation-enabled (y-or-n-p "Enable NAVIGATION module? "))
      (setq nemacs-module-term-enabled (y-or-n-p "Enable TERM module? "))
      (setq nemacs-module-wm-enabled (y-or-n-p "Enable WINDOW MANAGER module? "))

      (nemacs-write-settings)
      (setq nemacs-reset-setup-force nil)
      (kill-buffer "*Messages*"))))

(defun nemacs-reset-setup ()
  (interactive)
  "Resets the value of `nemacs-setup-complete' and runs the NEMACS setup."
  (setq nemacs-reset-setup-force t)
  (nemacs-start-setup t))

(defun nemacs-write-settings ()
  "Writes settings to the settings.el file."
  (write-region "" nil nemacs-settings-file)
  (append-to-file "(setq nemacs-setup-complete t)\n" nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-email-enabled %s)\n" nemacs-module-email-enabled) nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-finance-enabled %s)\n" nemacs-module-finance-enabled) nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-gtd-enabled %s)\n" nemacs-module-gtd-enabled) nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-navigation-enabled %s)\n" nemacs-module-navigation-enabled) nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-term-enabled %s)\n" nemacs-module-term-enabled) nil nemacs-settings-file)
  (append-to-file (format "(setq nemacs-module-wm-enabled %s)\n" nemacs-module-wm-enabled) nil nemacs-settings-file))
