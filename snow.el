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

(defcustom snow-time-delta 0.5
  "Time delay in seconds between steps of the snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-flake-threshold 0.08
  "Percent chance that a character will become a snowflake."
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index-delta 0.1
  "Crosson index delta applied to the crosson index of the
snowstorm, a measurement of simulated-snowstorm intensity."
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index 0.0
  "Crosson Index of the current snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-seed nil
  "Seed for the snow RNG. Nil means use a random seed."
  :type 'string
  :group 'snow)

(defcustom snow-initialized nil
  "Set to true when `snow' has been run once."
  :type 'boolean
  :group 'snow)

;;;###autoload
(defun snow ()
  "Simulate snow."
  (interactive)
  (snow-setup)
  (random snow-seed)
  (catch 'persephones-return
    (let* ((seed (random))
	   (cindex snow-crosson-index)
	   (snowflakes (list (snow-spawn cindex seed))))
      (while t
	(let ((inhibit-quit t)
	      (inhibit-read-only t))
	  (snow-display snowflakes snow-time-delta)
	  (setq cindex (snow-update-cindex cindex seed))
	  (setq seed (random))
	  (setq snowflakes (snow-fall snowflakes cindex seed)))))))

(define-derived-mode snow-mode special-mode "Snow"
  "Major mode for the buffer of `snow'."
  (setq-local case-fold-search nil)
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil)
  (setq-local fill-column (1- (window-width)))
  (setq-local snow-seed snow-seed)
  (setq-local mode-line-buffer-identification '("snow: " snow-seed))
  (buffer-disable-undo))

(defun snow-setup ()
  "Setup the snow simulation environment."
  (switch-to-buffer (get-buffer-create "*Snow*") t)
  (read-only-mode -1)
  (erase-buffer)
  (snow-mode)
  (when (not snow-initialized)
    ;; todo: update
    (setq snow-seed "zebulon")))

(defun snow-chance (percent)
  "Return true PERCENT% of the time, where PERCENT is a fraction
less than or equal to 1."
  (> percent (/ (random 100) 100.0)))

(defun snow-flake? (chance)
  "Function to guard snowlake creation based on SEED."
  (let ((create-seed? (snow-chance chance)))
    (if create-seed?
	snow-flake
      snow-noflake)))

(defun snow-spawn (crosson-index seed)
  "Spawn a horizontal slice of a snowstorm."
  (let* ((cols  (window-body-width))
	 (lines (window-body-height))
	 (snowflakes nil))
    (random seed)
    (dotimes (col cols snowflakes)
      (setq snowflakes (concat snowflakes (snow-flake? snow-flake-threshold))))))

(defun snow-fall (snowflakes crosson-index seed)
  "Compute the mutating state of SNOWFLAKES using SEED as a random number generator."
  (let ((new-snow (snow-spawn crosson-index seed))
	(local-snowflakes snowflakes))
    (when (< (window-total-height) (length local-snowflakes))
	(delq (last local-snowflakes) local-snowflakes))
    (cons new-snow local-snowflakes)))

(defun snow-update-cindex (crosson-index seed)
  "Mutate CINDEX into a new crosson-index based on rng SEED."
  (random seed)
  (abs (if (integerp (/ (random) 100))
	   (+ crosson-index (random snow-crosson-index-delta))
	 crosson-index)))

(defun snow-display (snowflakes sleeptime)
  ;; prep point
  (erase-buffer)
  (goto-char 1)
  (recenter 0)
  ;; insert snow
  (let ((flakes snowflakes))
    (while flakes
      (insert (concat (pop flakes) "\n"))))
  (goto-char 1)
  ;; time delta
  (or (and (sit-for sleeptime) (< 0 sleeptime))
      (not (input-pending-p))
      (throw 'persephones-return nil)))
