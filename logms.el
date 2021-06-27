;;; logms.el --- See where the message came from  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-26 23:22:27

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: See where the message came from
;; Keyword:
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/logms

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; See where the message came from.
;;

;;; Code:

(require 'button)

(defgroup logms nil
  "See where the message came from."
  :prefix "logms-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/logms"))

(defcustom logms-show t
  "Enable to show the button infront of the log message."
  :type 'boolean
  :group 'logms)

(defmacro logms-with-messages-buffer (&rest body)
  "Execute BODY within the *Messages* buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer "*Messages*"
     (let (buffer-read-only) (progn ,@body))))

(defun logms--next-msg-point ()
  "Return max point in *Messages* buffer."
  (logms-with-messages-buffer (point-max)))

(defun logms--make-button (beg end source pt)
  "Make a button from BEG to END.
Argument SOURCE is the buffer prints the log.
Argument PT indicates where the log beging print inside SOURCE buffer."
  (ignore-errors
    (make-text-button beg end 'follow-link t
                      'action (lambda (&rest _)
                                (if (not (buffer-live-p source))
                                    (user-error "Buffer no longer exists: %s" source)
                                  (switch-to-buffer-other-window source)
                                  (goto-char pt))))))

;;;###autoload
(defun logms (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (if logms-show
      (let* ((name (buffer-name))
             (source (current-buffer)) (pt (point))
             (display (format "%s:%s:%s" name (line-number-at-pos pt) (current-column)))
             (display-len (length display))
             (beg (logms--next-msg-point)))
        (apply 'message (concat "%s " fmt) display args)
        (logms-with-messages-buffer
          (unless (logms--make-button beg (+ beg display-len) source pt)
            (setq beg (save-excursion
                        (goto-char beg)
                        (when (= (line-beginning-position) (point-max))
                          (forward-line -1))
                        (line-beginning-position)))
            (logms--make-button beg (+ beg display-len) source pt))))
    (apply 'message fmt args)))

(provide 'logms)
;;; logms.el ends here
