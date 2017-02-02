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

;; Also thanks to life.el

;; Usage:

;; M-x snow
;;   - or -
;; (snow)

;; to enter a specific seed, <seed>:
;; C-u M-x snow RET <seed> RET

;; Note: the seed is visible in the mode line of a buffer in
;; `snow-mode'.

;;; Code:

(require 'gamegrid)

(defgroup snow nil
  "Snow simulation."
  :group 'games)

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

(defcustom snow-crosson-index-delta 0.01
  "The delta applied to the current Crosson index of the snowstorm, a
measurement of simulated-snowstorm intensity."
  :type 'number
  :group 'snow)

(defcustom snow-crosson-index 0.0
  "Crosson Index of the current snowstorm."
  :type 'number
  :group 'snow)

(defcustom snow-score-file "~/.emacs.d/games/snow"
  "File containing the high score (runtime) records of `snow'
storms."
  :type 'file
  :group 'snow)

(defcustom snow-seed nil
  "Random seed to use for this round of snow simulation."
  :type 'number
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

(defcustom snow-debug-status nil
  "Debug mode status of function `snow'."
  :type 'boolean
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

;; todo: more stability but sudden surges in cindex
(defun snow-update-cindex (crosson-index)
  "Mutate CROSSON-INDEX into a new crosson-index pseudo-randomly."
  (abs (+ crosson-index (* (- (random 10) 7) snow-crosson-index-delta))))

(defun snow-increment-timeslice ()
  "Increment variable `snow-timeslice' and corresponding
variable `snow-timeslice-string'."
  (snow-increment snow-timeslice)
  (setq-local snow-timeslice-string (concat
				     (int-to-string snow-timeslice)
				     ":" snow-seed))
  (setq-local snow-crosson-index (snow-update-cindex snow-crosson-index)))

(defun snow-debug ()
  "Display debugging information if `snow-debug-status' is
non-nil."
  (message (format "%g cindex" snow-crosson-index)))

(defun snow-spawn (crosson-index)
  "Spawn a horizontal slice of a snowstorm.
Argument CROSSON-INDEX is the current intensity of the storm."
  (let* ((cols  (window-body-width))
	 (lines (window-body-height))
	 (snowflakes nil))
    (dotimes (col cols snowflakes)
      (setq snowflakes (concat snowflakes (snow-flake? snow-flake-threshold))))))

(defun snow-fall (snowflakes crosson-index)
  "Compute the mutating state of SNOWFLAKES.
Argument CROSSON-INDEX is the intensity of the current storm."
  (let ((new-snow (snow-spawn crosson-index))
	(local-snowflakes snowflakes))
    (when (< (window-total-height) (length local-snowflakes))
      (nbutlast local-snowflakes 1))
    (cons new-snow local-snowflakes)))

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
  "Display SNOWFLAKES and monitor for user input. If input is
pending after SLEEPTIME seconds, exit `snow-mode.'"
  (snow-insert snowflakes)
  (snow-debug)
  (or (and (< 0 sleeptime) (sit-for sleeptime))
      (not (input-pending-p))
      (throw 'persephones-return nil)))

(defun snow-seconds-to-human-time-string (seconds)
  "Convert SECONDS into a human-readable time string of the
following format: 02:10:01."
  (let ((hours 0)
	(minutes 0))
    (while (<= 0 (- seconds (* 60 60)))
      (setq seconds (- seconds (* 60 60)))
      (snow-increment hours))
    (while (<= 0 (- seconds 60))
      (setq seconds (- seconds 60))
      (snow-increment minutes))
    (format "%02d:%02d:%02d" hours minutes seconds)))

(defun snow-random-runtime-message ()
  "Evaluate to a format string containing one %s."
  ;; could arrange for loose tiering of (weighted rand and ordered
  ;; list) of messages
  (let* ((messages `("You were snowed in for %s."
		    "Brrr! You braved that snowstorm for %s."
		    "%s of snow! Are you completely submerged yet?"
		    ,(format "After %%s of searching, the rescue party found %s!"
			     user-login-name)))
	 (rand (random (length messages))))
    (dotimes (i rand (car messages))
      (pop messages))))

(defun snow-display-runtime (seconds)
  "Display a message containing SECONDS, the length of the
snowstorm the user witnessed, in human readable form."
  (message (snow-random-runtime-message)
	   (snow-seconds-to-human-time-string seconds)))

(defun snow-record-score (seconds)
  "Records SECONDS in variable `snow-score-file', if the dir
containing it exists."
  (when (file-exists-p (file-name-directory snow-score-file))
    (gamegrid-add-score snow-score-file seconds)))

(defun snow-setup ()
  "Setup the snow simulation environment."
  (switch-to-buffer (get-buffer-create "*Snow*") t)
  (read-only-mode -1)
  (erase-buffer)
  (snow-mode)
  (setq-local snow-seed (snow-random-seed))
  (setq-local snow-crosson-index snow-crosson-index)
  (setq-local snow-timeslice 0)
  (setq-local snow-timeslice-string (concat "0:" snow-seed)))

(define-derived-mode snow-mode special-mode "Snow"
  "Major mode for the buffer of `snow'."
  (setq-local case-fold-search nil)
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil)
  (setq-local fill-column (1- (window-width)))
  (setq-local mode-line-buffer-identification '("snow: " snow-timeslice-string))
  (buffer-disable-undo))

;;;###autoload
(defun snow (&optional prefix)
  "Simulate snow.

C-u PREFIX prompts the user for a specific seed."
  (interactive "p")
  (let ((user-seed nil))
    (when (eq prefix 4)
      (setq user-seed (string-to-number
		       (read-from-minibuffer "Seed: "))))
    (snow-setup)
    (random (or user-seed snow-seed))
    (let ((seconds
	   (car
	    (benchmark-run 1
	      (catch 'persephones-return
		(let* ((crosson-index snow-crosson-index)
		       (snowflakes (list (snow-spawn crosson-index))))
		  (while t
		    (let ((inhibit-quit t)
			  (inhibit-read-only t))
		      (snow-display snowflakes snow-time-delta)
		      (setq crosson-index (snow-update-cindex crosson-index))
		      (setq snowflakes (snow-fall snowflakes crosson-index))
		      (snow-increment-timeslice)))))))))
      (snow-display-runtime seconds)
      ;; todo: run this without it hijacking the minibuffer and
      ;; current buffer
      ; (snow-record-score seconds)
      )))

(provide 'snow)

;;; snow.el ends here
