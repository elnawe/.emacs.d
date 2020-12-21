;;; core/interface.el --- NEMACS CORE Interface File.

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

(require 'battery)

;;
;;; MODE LINE

;; Display the battery if available/user is in laptop.
(when IS-LAPTOP
  (setq battery-update-interval 15
        fancy-battery-show-percentage t)
  (display-battery-mode 1))
;; Display the time
(setq display-time-24h-format t
      display-time-default-load-average nil
      display-time-format "%b %d %H:%M ")
(display-time-mode 1)
;; Hide help echo from the modeline.
(setq mode-line-default-help-echo nil)
;; Ask for `y-or-n' instead of `yes-or-no'.
(fset #'yes-or-no-p #'y-or-n-p)

;;
;;; FRAMES/WINDOWS

;; General and frame configurations
(setq-default frame-inhibit-implied-resize t
	      frame-title-format "NEMACS"
	      fringe-indicator-alist (delq
				      (assq 'continuation fringe-indicator-alist)
				      fringe-indicator-alist)
	      help-window-select t
	      resize-mini-windows 'grow-only
	      ring-bell-function #'ignore
	      show-help-function nil
	      split-height-threshold nil
	      split-width-threshold 160
	      uniquify-buffer-name-style 'post-forward-angle-brackets
	      use-dialog-box nil
	      visible-bell nil
	      word-wrap t)
;; Remove all frame distractions if it's not in terminal mode.
(when window-system
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;
;;; SCROLLING

;; Fix horizontal and vertical scrolling, specially in larger files. Scrolling in Emacs
;; is sometimes slow in larger files as it tries to center the view in
(setq auto-window-vscroll nil
      hscroll-margin 2
      hscroll-step 1
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t)

;;
;;; CURSOR

;; Blink cursor, it makes life simpler. This might conflict with some minor modes.
(blink-cursor-mode 1)
;; Don't blink the paren matching at point.
(setq blink-matching-paren nil)
;; Don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)
;; Show cursor only in current selected window.
(setq cursor-in-non-selected-windows nil)
