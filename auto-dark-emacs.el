;;; auto-dark-eamcs.el --- Automatically set the dark-mode state of Emacs

;; Copyright (C) 2019 Rahul M. Juliato

;; Licensed under the same terms as Emacs.

;; Author: Rahul M. Juliato
;;      Tim Harper <timcharper at gmail dot com>
;; Maintainer: Please send bug reports to the github site (below).
;; Created: July 16 2019
;; Version: 0.2
;; Keywords: gui, os-integration
;; URL: https://github.com/LionyxML/auto-dark-emacs
;;
;; This file is not part of GNU Emacs.
;;
;; Copy some codes from https://github.com/hadronzoo/theme-changer,
;; Thanks for Joshua Griffith (hadronzoo@github.com).

;;; Commentary:

;; Auto-Dark is an auto-changer between 2 themes, dark/light, respecting the
;; overall settings of MacOS. To enable it, install the package, add it to your
;; load path, and:
;;
;;     (require 'auto-dark-emacs)
;;
;; To customize the themes used by light / dark mode:
;;
;;     M-x customize-group auto-dark-emacs

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'solar)

(defcustom auto-dark-emacs/dark-theme 'wombat
  "The theme to enable when dark-mode is active."
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/light-theme 'leuven
  "The theme to enable when dark-mode is inactive."
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/polling-interval-seconds 5
  "The number of seconds between which to poll for dark mode state.
Emacs must be restarted for this value to take effect"
  :group 'auto-dark-emacs
  :type 'integer)

(defvar auto-dark-emacs/last-dark-mode-state nil)

(defun auto-dark-emacs/hour-fraction-to-time (date hour-fraction)
  "Merge fract time to the full time form.

DATE is a Gregorian DATE.
HOUR-FRACTION is List of *local* times."
  (let*
      ((now (decode-time (current-time)))

       (month (first   date))
       (day   (second  date))
       (year  (third   date))
       (zone  (ninth   now))

       (frac-hour (cl-truncate hour-fraction))
       (hour (first frac-hour))

       (frac-minutes (cl-truncate (* (second frac-hour) 60)))
       (minute (first frac-minutes))

       (frac-seconds (cl-truncate (* (second frac-minutes) 60)))
       (sec (first frac-seconds)))
    (encode-time sec minute hour day month year zone)))


(defun auto-dark-emacs/sunrise-sunset-times (date)
  "Get sunrise/sunset time of the special day.

DATE is a Gregorian DATE.
If the sunrise time later than 6:00, set it to 6:00.
If the sunset time later than 19:00, set it to 19:00."
  (let*
      ((l (solar-sunrise-sunset date))
       (sunrise-time (auto-dark-emacs/hour-fraction-to-time date (min 6.0 (caar l))))
       (sunset-time (auto-dark-emacs/hour-fraction-to-time date (min 19.0 (caadr l)))))
    (if (> emacs-major-version 26)
        (list (encode-time (decode-time sunrise-time))
              (encode-time (decode-time sunset-time)))
      (list sunrise-time sunset-time))))

(defun auto-dark-emacs/today ()
  "Return current date as a list."
  (calendar-current-date))

(defun auto-dark-emacs/tomorrow ()
  "Return next date as a list."
  (calendar-gregorian-from-absolute
   (1+ (calendar-absolute-from-gregorian (auto-dark-emacs/today)))))

(defun auto-dark-emacs/is-dark-mode-builtin ()
  "Check if the dark mode is enabled.
Invoke applescript using Emacs built-in AppleScript support to
see if dark mode is enabled. Returns true if it is."
  (let ((cmd-res (replace-regexp-in-string "\"" ""
                   (do-applescript
                    "tell application \"System Events\"
	tell appearance preferences
		if (dark mode) then
			return \"true\"
		else
			return \"false\"
		end if
	end tell
end tell"))))
    (if (and cmd-res
             (or (string-equal cmd-res "true")
                 (string-equal cmd-res "false")))
        cmd-res
      nil)))

(defun auto-dark-emacs/is-dark-mode-osascript ()
  "Invoke applescript using external shell command.
this is less efficient, but works for non-GUI Emacs"
  (let ((cmd-res (string-trim
                  (shell-command-to-string
                   "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))
    (if (and cmd-res
             (or (string-equal cmd-res "true")
                 (string-equal cmd-res "false")))
        cmd-res
      nil)))

(defun auto-dark-emacs/is-dark-mode ()
  "Whether if the dark mode is enabled.
If supported, invoke applescript using Emacs built-in
AppleScript support to see if dark mode is enabled. Otherwise,
check dark-mode status using osascript."
  (if (fboundp 'do-applescript)
      (auto-dark-emacs/is-dark-mode-builtin)
    (auto-dark-emacs/is-dark-mode-osascript)))

(defun auto-dark-emacs/check-and-set-dark-mode ()
  "Set the theme according to Mac OS's dark mode state.
In order to prevent flickering, we only set the theme if we haven't
already set the theme for the current dark mode state."
  ;; Get's MacOS dark mode state
  (let ((is-dark-mode (auto-dark-emacs/is-dark-mode)))
    (if (or (not is-dark-mode)
            (and auto-dark-emacs/last-dark-mode-state
                 (string-equal is-dark-mode auto-dark-emacs/last-dark-mode-state)))
        (run-with-idle-timer auto-dark-emacs/polling-interval-seconds nil
                             #'auto-dark-emacs/check-and-set-dark-mode)
      (mapc #'disable-theme custom-enabled-themes)
      (if (string-equal is-dark-mode "true")
          (load-theme (if (listp auto-dark-emacs/dark-theme)
                          (elt auto-dark-emacs/dark-theme
                               (random (length auto-dark-emacs/dark-theme)))
                        auto-dark-emacs/dark-theme)
                      t)
        (load-theme (if (listp auto-dark-emacs/light-theme)
                        (elt auto-dark-emacs/light-theme
                             (random (length auto-dark-emacs/light-theme)))
                      auto-dark-emacs/light-theme)
                    t))
      (setq auto-dark-emacs/last-dark-mode-state is-dark-mode)
      (let* ((now (current-time))
             (today-sunrise-sunset (auto-dark-emacs/sunrise-sunset-times
                                    (auto-dark-emacs/today)))
             (sunrise-today (car today-sunrise-sunset))
             (sunset-today (cadr today-sunrise-sunset))
             (sunrise-tomorrow (car (auto-dark-emacs/sunrise-sunset-times
                                     (auto-dark-emacs/tomorrow))))
             (next-change (cond ((time-less-p now sunrise-today) sunrise-today)
                                ((time-less-p now sunset-today) sunset-today)
                                (t sunrise-tomorrow))))
        (if (or (and (time-equal-p next-change sunset-today)
                     (string-equal is-dark-mode "true"))
                (and (time-equal-p next-change sunrise-tomorrow)
                     (string-equal is-dark-mode "false")))
            (run-with-idle-timer auto-dark-emacs/polling-interval-seconds nil
                                 #'auto-dark-emacs/check-and-set-dark-mode)
          (run-at-time next-change nil
                       #'auto-dark-emacs/check-and-set-dark-mode))))))


(provide 'auto-dark-emacs)

;;; auto-dark-emacs.el ends here
