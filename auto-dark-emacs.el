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
;; overall settings of MacOS. To enable it, install the package, add it to your load path, and:
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
  "The theme to enable when dark-mode is active"
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/light-theme 'leuven
  "The theme to enable when dark-mode is inactive"
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/polling-interval-seconds 5
  "The number of seconds between which to poll for dark mode state. Emacs must be restarted for this value to take effect"
  :group 'auto-dark-emacs
  :type 'integer)

(defcustom auto-dark-emacs/allow-osascript nil
  "Whether to allow auto-dark-mode to shell out to osascript to check dark-mode state, if ns-do-applescript is not available"
  :group 'auto-dark-emacs
  :type 'boolean)

(defvar auto-dark-emacs/last-dark-mode-state nil)

(defun auto-dark-emacs/hour-fraction-to-time (date hour-fraction)
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
  (let*
      ((l (solar-sunrise-sunset date))
       (sunrise-time (auto-dark-emacs/hour-fraction-to-time date (caar l)))
       (sunset-time (auto-dark-emacs/hour-fraction-to-time date (caadr l))))
    (if (> emacs-major-version 26)
        (list (encode-time (decode-time sunrise-time))
              (encode-time (decode-time sunset-time)))
      (list sunrise-time sunset-time))))

(defun auto-dark-emacs/today () (calendar-current-date))

(defun auto-dark-emacs/tomorrow ()
  (calendar-gregorian-from-absolute
   (+ 1 (calendar-absolute-from-gregorian (auto-dark-emacs/today)))))

(defun auto-dark-emacs/is-dark-mode-builtin ()
  "Invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled. Returns true if it is."

  (string-equal "true" (ns-do-applescript "tell application \"System Events\"
  tell appearance preferences
    if (dark mode) then
      return \"true\"
    else
      return \"false\"
    end if
  end tell
end tell")))

(defun auto-dark-emacs/is-dark-mode-osascript ()
  "Invoke applescript using Emacs using external shell command; this is less efficient, but works for non-GUI emacs"

  (string-equal "true" (string-trim (shell-command-to-string "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))

(defun auto-dark-emacs/is-dark-mode ()
  "If supported, invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled. Otherwise, check dark-mode status using osascript, if allowed by auto-dark-emacs/allow-osascript."

  (if (fboundp 'ns-do-applescript)
      (auto-dark-emacs/is-dark-mode-builtin)

    (and auto-dark-emacs/allow-osascript (auto-dark-emacs/is-dark-mode-osascript))))

(defun auto-dark-emacs/check-and-set-dark-mode ()
  "Sets the theme according to Mac OS's dark mode state. In order to prevent flickering, we only set the theme if we haven't already set the theme for the current dark mode state."
  ;; Get's MacOS dark mode state
  (let ((is-dark-mode (auto-dark-emacs/is-dark-mode)))
    (if (and auto-dark-emacs/last-dark-mode-state
             (eq is-dark-mode auto-dark-emacs/last-dark-mode-state))
        (run-with-idle-timer auto-dark-emacs/polling-interval-seconds nil
                             #'auto-dark-emacs/check-and-set-dark-mode)
      (setq auto-dark-emacs/last-dark-mode-state is-dark-mode)
      (mapc #'disable-theme custom-enabled-themes)
      (if is-dark-mode
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
        (run-at-time next-change nil
                     #'auto-dark-emacs/check-and-set-dark-mode)))))


(provide 'auto-dark-emacs)
