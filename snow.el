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

;; Modeled after a bash script I found somewhere.  I'll try to find the
;; source as this grows.

;; TODOs:

;; - [X] snow fall
;; - [ ] pickable rng
;; - [ ] snow collects on ground
;; - [ ] fastforward
;; - [ ] rewind
;; - [ ] jump to time
;; - [ ] transpose flurries
;; - [ ] wind gusts
;; - [ ] ground clusters
;; - [ ] worsening conditions on the cloud
;; - [ ] recognized keys in snow-mode
;; - [ ] hide snow-mode as an interactive function
;; - [ ] keep track of time snowed in
;; - [ ] achievements unlocked

;; Usage:

;; M-x snow
;;   - or -
;; (snow)

;;; Code:

(defcustom snow-flake "❄"
  "ASCII snowflake.  Each one is unique!"
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

(defcustom snow-flake-threshold 0.002
  "Percent chance that a character will become a snowflake."
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index-delta 0.1
  "The delta applied to the current Crosson index of the snowstorm, a
measurement of simulated-snowstorm intensity."
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index 0.0
  "Crosson Index of the current snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-seed nil
  "Seed for the snow RNG.  Nil means use a random seed."
  :type 'string
  :group 'snow)

(defcustom snow-initialized nil
  "Set to true when `snow' has been run once."
  :type 'boolean
  :group 'snow)

(defcustom snow-timeslice nil
  "Current time slice of the observed snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-timeslice-string nil
  "Current time slice of the observed snowstorm."
  :type 'string
  :group 'snow)

;; Macros are used macros for manifest constants instead of variables
;; because the compiler will convert them to constants, which should
;; eval faster than symbols.

(defmacro snow-increment (variable) (list 'setq variable (list '1+ variable)))

(defun snow-chance (percent)
  "True PERCENT% of the time, with some variance."
  (let* ((range 10)
	 (variance range)
	 (variance-denominator (* range 100))
	 (variance-rand (- (random variance) range))
	 (offset (/ variance-rand variance-denominator)))
    (> percent (/ (+ (random 100) offset) 100.0))))

(defun snow-flake? (chance)
  "Function to guard snowlake creation.
Argument CHANCE is the likelihood that a given char will be a
snowflake."
  (if (snow-chance chance)
      snow-flake
    snow-noflake))

(defun snow-random-seed ()
  "A random string used as the current snowstorm's RNG seed."
  (let* ((seed-length 8)
	 (alphabet "abcdefghijklmnopqrstuvwxyz")
	 (digits "0123456789")
	 (symbols "!@#$^&*()=+-")
	 (dict (concat alphabet (upcase alphabet) digits symbols))
	 (seed ""))
    (dotimes (i seed-length seed)
      (let ((index (random (length dict))))
	(setq seed (concat seed (substring-no-properties
				 dict index (+ 1 index))))))))

(defun snow-increment-timeslice ()
  "Increment variable `snow-timeslice' and corresponding
variable `snow-timeslice-string'."
  (snow-increment snow-timeslice)
  (setq-local snow-timeslice-string (concat
				     (int-to-string snow-timeslice)
				     ":" snow-seed)))

(defun snow-setup ()
  "Setup the snow simulation environment."
  (switch-to-buffer (get-buffer-create "*Snow*") t)
  (read-only-mode -1)
  (erase-buffer)
  (snow-mode)
  (when (not snow-initialized)
    ;; todo: update
    (setq snow-seed (snow-random-seed)))
  (setq-local snow-timeslice 0)
  (setq-local snow-timeslice-string (concat "0:" snow-seed)))

(defun snow-spawn (crosson-index seed)
  "Spawn a horizontal slice of a snowstorm.
Argument CROSSON-INDEX is the current intensity of the storm.
Argument SEED is a seed for the RNG."
  (let* ((cols  (window-body-width))
	 (lines (window-body-height))
	 (snowflakes nil))
    (random seed)
    (dotimes (col cols snowflakes)
      (setq snowflakes (concat snowflakes (snow-flake? snow-flake-threshold))))))

(defun snow-fall (snowflakes crosson-index seed)
  "Compute the mutating state of SNOWFLAKES using SEED as a
random number generator.
Argument CROSSON-INDEX is the intensity of the current storm."
  (let ((new-snow (snow-spawn crosson-index seed))
	(local-snowflakes snowflakes))
    (when (< (window-total-height) (length local-snowflakes))
	(nbutlast local-snowflakes 1))
    (cons new-snow local-snowflakes)))

(defun snow-update-cindex (crosson-index seed)
  "Mutate CROSSON-INDEX into a new crosson-index based on rng SEED."
  (random seed)
  (abs (if (integerp (/ (random) 100))
	   (+ crosson-index (random snow-crosson-index-delta))
	 crosson-index)))

(defun snow-insert (snowflakes)
  "Insert SNOWFLAKES into the current buffer at point."
  (let ((inhibit-quit t)
	(inhibit-read-only t))
    ;; prep point
    (erase-buffer)
    (goto-char 1)
    (recenter 0)
    ;; insert snow
    (let ((flakes snowflakes))
      (while flakes
	(insert (concat (pop flakes) "\n"))))
    (goto-char 1)
    (recenter 0)))

(defun snow-display (snowflakes sleeptime)
  "Display SNOWFLAKES and monitor the user for input. If input is
pending after SLEEPTIME seconds, exit `snow-mode.'"
  (snow-insert snowflakes)
  ;; time delta
  (or (and (< 0 sleeptime) (sit-for sleeptime))
      (not (input-pending-p))
      (throw 'persephones-return nil)))

(define-derived-mode snow-mode special-mode "Snow"
  "Major mode for the buffer of `snow'."
  (setq-local case-fold-search nil)
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil)
  (setq-local fill-column (1- (window-width)))
  (setq-local snow-seed snow-seed)
  (setq-local mode-line-buffer-identification
	      '("snow: " snow-timeslice-string))
  (buffer-disable-undo))

;;;###autoload
(defun snow ()
  "Simulate snow."
  (interactive)
  (snow-setup)
  (random snow-seed)
  (catch 'persephones-return
    (let* ((rand (random))
	   (cindex snow-crosson-index)
	   (snowflakes (list (snow-spawn cindex rand))))
      (while t
	(let ((inhibit-quit t)
	      (inhibit-read-only t))
	  (snow-display snowflakes snow-time-delta)
	  (setq rand (random))
	  (setq cindex (snow-update-cindex cindex rand))
	  (setq snowflakes (snow-fall snowflakes cindex rand))
	  (snow-increment-timeslice))))))

(provide 'snow)

;;; snow.el ends here
