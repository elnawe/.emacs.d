;;; init.el --- NEMACS Initialization File.

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

(dolist (file '("vars.el"
                "preload.el"
                "packages.el"
                "keybindings.el"
                "hooks.el"
                "theme.el"))
  (load (concat user-emacs-directory file)))

(add-hook 'after-init-hook
          #'(lambda ()
              (dolist (module (directory-files nemacs-modules-dir))
                (when (string-match (format "^\\(.+\\)\\.module\\.el$") module)
                  (message "Loading " module)
                  (load (concat nemacs-modules-dir module))))

              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)

              (require 'nemacs-ensure-system)
              (nemacs-ensure-system-check-errors)))
