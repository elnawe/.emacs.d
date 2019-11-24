;;; nemacs-fn-keys.el --- NEMACS Function Keys.

;; Controls the `Function' Keys in NEMACS.

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

(defvar nemacs-exwm-fn-key-controller
  (expand-file-name "dotfiles/bin/fn_key_control" nemacs-dropbox-dir)
  "Script that controls all the `Function' Keys in the keyboard.")

(defun nemacs-fn-key-mute-volume ()
  "Mute the system volume."
  (interactive)
  (shell-command (concat nemacs-exwm-fn-key-controller " --mute-volume")))

(defun nemacs-fn-key-decrease-volume ()
  "Decrease the system volume."
  (interactive)
  (shell-command (concat nemacs-exwm-fn-key-controller " --vol-dec")))

(defun nemacs-fn-key-increase-volume ()
  "Increase the system volume."
  (interactive)
  (shell-command (concat nemacs-exwm-fn-key-controller " --vol-inc")))

(defun nemacs-fn-key-mute-microphone ()
  "Mute the system microphone."
  (interactive)
  (shell-command (concat nemacs-exwm-fn-key-controller " --mute-mic")))

(provide 'nemacs-fn-keys)
