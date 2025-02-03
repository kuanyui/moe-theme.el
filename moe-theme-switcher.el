;; moe-theme-switcher.el

;; Copyright (C) 2013-2022 kuanyui

;; Author: kuanyui (azazabc123@gmail.com)
;; Date: 2013/05/11 11:39
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; TODO: This program need to be refactored.
(require 'moe-dark-theme)
(require 'moe-light-theme)
(require 'moe-theme)
(require 'solar)

(defvar moe-theme-switch-by-sunrise-and-sunset t
"Automatically switch between dark and light moe-theme.

If this value is nil, moe-theme will switch at fixed time (06:00 and 18:00).

If this value is t and both calendar-latitude and calendar-longitude are set
properly, the switching will be triggered at the sunrise and sunset time of the
local calendar.

Take Keelung, Taiwan(25N,121E) for example, you can set like this:

	(setq calendar-latitude +25)
	(setq calendar-longitude +121)")

(defvar moe-theme-switcher--which-enabled nil
  "Variable indicate which theme (moe-dark or light) is being used.")

(defvar moe-theme-switcher--24h/sunrise)

(defvar moe-theme-switcher--24h/sunset)

(defvar moe-theme-switcher--compute-sunrise-sunset-timer nil
  "Timer for updating the computed sunrise and sunset times.")

(defvar moe-theme-switcher--timer nil
  "Timer for checking whether to switch themes between `moe-light' and `moe-dark'.")

(defun moe-theme-switcher--load-theme (switch-to)
  "Avoid unnecessary load-theme and screen flashing in GUI version Emacs"
  (cond ((equal switch-to 'light)
         (if (not (equal moe-theme-switcher--which-enabled 'light))
           (progn (moe-light)
                  (setq moe-theme-switcher--which-enabled 'light)))) ;[FIXME] Maybe unnecessary
        ((equal switch-to 'dark)
         (if (not (equal moe-theme-switcher--which-enabled 'dark))
           (progn (moe-dark)
                  (setq moe-theme-switcher--which-enabled 'dark)))))) ;[FIXME] Maybe unnecessary

(defun moe-theme-switcher--switch-at-fixed-time ()
  (let ((now (string-to-number (format-time-string "%H"))))
    (if (and (>= now 06) (<= now 18))
        (moe-theme-switcher--load-theme 'light)
      (moe-theme-switcher--load-theme 'dark))
    nil))

;; (Thanks for letoh!)
;; Fix strange bahavior of sunrise-sunset when buffer's width is too narrow.
(defun moe-theme-switcher--get-sunrise-sunset-string ()
  "get the real result from `sunrise-sunset'"
  (save-window-excursion
    (let ((regex "[0-9]+:[0-9]+[ap]m")
          (s (sunrise-sunset))
          (buf (get-buffer "*temp*")))
      (unless (and (stringp s)
                   (string-match-p regex s))
        (when buf
          (with-current-buffer buf
            (let* ((s1 (buffer-string))
                   (s2 (if (string-match-p regex s1)
                           s1 nil)))
              (setq s s2)
              (kill-buffer buf)))))
      s)))

;; Convert am/pm to 24hr and save to 24h/sunrise & 24h/set
;; Excute every 24 hr
(defun moe-theme-switcher--compute-sunrise-sunset ()
  (let (rise_set a b c d e f)
    (setq rise_set (moe-theme-switcher--get-sunrise-sunset-string))
    (if (string-match "0:00 hours daylight" rise_set) ;If polar-night
        (progn
          (setq moe-theme-switcher--24h/sunrise 'polar-night
                moe-theme-switcher--24h/sunset 'polar-night))
      (if (string-match "24:00 hours daylight" rise_set) ;If midnight-sun
          (progn
            (setq moe-theme-switcher--24h/sunrise 'midnight-sun
                  moe-theme-switcher--24h/sunset 'midnight-sun))
        (progn                          ;Convert 12hr to 24hr
          (string-match "\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\).+\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\)" rise_set)
          (setq a (string-to-number (match-string 1 rise_set))
                b (string-to-number (match-string 2 rise_set))
               c (match-string 3 rise_set)
               d (string-to-number (match-string 4 rise_set))
               e (string-to-number (match-string 5 rise_set))
               f (match-string 6 rise_set))
         (if (equal c "pm")
             (setq moe-theme-switcher--24h/sunrise (list (+ 12 a) b))
           (setq moe-theme-switcher--24h/sunrise (list a b)))
         (if (equal f "pm")
             (setq moe-theme-switcher--24h/sunset (list (+ 12 d) e))
           (setq moe-theme-switcher--24h/sunset (list d e))))))))

;; Excute every minute.
(defun moe-theme-switcher--switch-by-locale ()
  (if (equal moe-theme-switcher--24h/sunrise 'polar-night)  ;If polar-night...moe-dark!
      (moe-theme-switcher--load-theme 'dark)
    (if (equal moe-theme-switcher--24h/sunrise 'midnight-sun) ;If midnight-sun...moe-light!
        (moe-theme-switcher--load-theme 'light)
      (progn
       (let ((now (list (string-to-number (format-time-string "%H"))
                        (string-to-number (format-time-string "%M")))))
         (if (and (or                        ;magic-logic [tm]
                   (> (car now) (car moe-theme-switcher--24h/sunrise))
                   (and
                    (= (car now) (car moe-theme-switcher--24h/sunrise))
                    (>= (cadr now) (cadr moe-theme-switcher--24h/sunrise))))
                  (or
                   (< (car now) (car moe-theme-switcher--24h/sunset))
                   (and
                    (= (car now) (car moe-theme-switcher--24h/sunset))
                    (< (cadr now) (cadr moe-theme-switcher--24h/sunset)))))
             (moe-theme-switcher--load-theme 'light)
           (moe-theme-switcher--load-theme 'dark)))))))

(defun moe-theme-switcher--auto-switch ()
  "Automatically switch between dark and light moe-theme."
  (interactive)
  (if (boundp 'moe-theme-switcher--24h/sunrise)
      (moe-theme-switcher--switch-by-locale)
    (moe-theme-switcher--switch-at-fixed-time)))

(defun moe-theme-switcher-enable ()
  "Enable automatic switching between the `moe-light' and `moe-dark'
themes according to the time of day.

If `moe-theme-switch-by-sunrise-and-sunset' is non-`nil', this
will use the values of `calendar-latitude' and
`caledar-longitude' to compute the sunrise and sunset.

Otherwise, switch the the theme at fixed times (06:00 and 18:00)."
  (interactive)
  (when (and
         (boundp 'calendar-longitude)
         (boundp 'calendar-latitude)
         moe-theme-switch-by-sunrise-and-sunset)
    (setq moe-theme-switcher--compute-sunrise-sunset-timer
          (run-with-timer 0 (* 60 60 24)
                          'moe-theme-switcher--compute-sunrise-sunset)))

  (setq moe-theme-switcher--timer (run-with-timer 0 (* 1 60) 'moe-theme-switcher--auto-switch)))

(defun moe-theme-switcher-disable ()
  "Disable automatic switching between the `moe-light' and
`moe-dark' themes."
  (interactive)
  (when (and (boundp 'moe-theme-switcher--compute-sunrise-sunset-timer)
             (timerp moe-theme-switcher--compute-sunrise-sunset-timer))
    (cancel-timer moe-theme-switcher--compute-sunrise-sunset-timer))
  (cancel-timer moe-theme-switcher--timer))

;;;###autoload
(define-minor-mode moe-theme-switcher-mode
  "Minor mode for enabling automatic switching between the
`moe-light' and `moe-dark' themes accoring to the time of day.

See the documentation for `moe-theme-switcher-enable' and
`moe-theme-switch-by-sunrise-and-sunset' for details."
  :lighter " Moe"
  :global t
  :group 'moe-theme-switcher
  (if moe-theme-switcher-mode
      (moe-theme-switcher-enable)
    (moe-theme-switcher-disable)))

(provide 'moe-theme-switcher)
