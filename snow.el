;;; snow.el --- Let it snow, let it snow, let it snow.
;; Version: 0.0.20140305

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: snow
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modeled after a bash script I found somewhere. I'll try to find the
;; source as this grows.

;; Usage:

;; M-x snow
;;   - or -
;; (snow)

;;; Code

(defcustom snow-flake "❄"
  "ASCII snowflake. Each one is unique!"
  :type 'string
  :group 'snow)

(defcustom snow-noflake " "
  "ASCII absence of a snowflake."
  :type 'string
  :group 'snow)

(defcustom snow-time-delta 0.1
  "ASCII snowflake. Each one is unique!"
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index 0.0
  "Crosson Index of the current snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-seed "zebulon"
  "Seed for the snow RNG. Nil means use a random seed."
  :type 'string
  :group 'snow)

(defun snow-flake? (seed)
  "Function to guard snowlake creation based on SEED."
  (let ((create-seed? (and (evenp seed)
			  (< 0 seed))))
    (if create-seed?
	snow-flake
      snow-noflake)))

(defun snow-spawn (crosson-index seed)
  "Spawn a snowstorm."
  (let* ((cols  (window-total-width))
	 (lines (window-total-height))
	 (snowflakes nil))
    (random seed)
    (dotimes (col cols snowflakes)
      (setq snowflakes (concat snowflakes (snow-flake? (random))))
      )))

(defun snow ()
  ""
  (interactive)
  (let* ((snowflakes (snow-spawn snow-crosson-index snow-seed)))
    (message snowflakes)
      ))
