;;; logms.el --- See where the message came from  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-26 23:22:27

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: See where the message came from
;; Keyword:
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4") (f "0.20.0") (s "1.9.0") (ht "2.3"))
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

(require 'backtrace)
(require 'button)
(require 'find-func)
(require 'subr-x)

(require 'f)
(require 's)
(require 'ht)

(defgroup logms nil
  "See where the message came from."
  :prefix "logms-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/logms"))

(defcustom logms-show t
  "Enable to show the button infront of the log message."
  :type 'boolean
  :group 'logms)

(defconst logms--search-context "(logms[ \t\"]*"
  "Regular expression to search for logms calls.")

(defvar logms--log-map (ht-create)
  "Record the log situation between from last to current command.

This resolved printing the same log in same frame level by acutally counting
the program execution.")

;;
;; (@* "Util" )
;;

(defun logms--inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (nth 4 (syntax-ppss)) (nth 8 (syntax-ppss))))

(defmacro logms-with-messages-buffer (&rest body)
  "Execute BODY within the *Messages* buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer "*Messages*"
     (let (buffer-read-only) (progn ,@body))))

(defun logms--count-symbols (symbol beg end)
  "Count the SYMBOL from region BEG to END."
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (search-forward symbol end t)
        (setq count (1+ count))))
    count))

(defun logms--nest-level-in-region (beg end)
  "Return the nest level from region BEG to END."
  (let ((opens (logms--count-symbols "(" beg end))
        (closes (logms--count-symbols ")" beg end)))
    (abs (- opens closes))))

(defun logms--nest-level-at-point ()
  "Return a integer indicates the nested level from current point."
  (let ((left (logms--nest-level-in-region (point-min) (point)))
        (right (logms--nest-level-in-region (point) (point-max))))
    (1- (/ (+ left right) 2))))

;;
;; (@* "Core" )
;;

(defun logms--next-msg-point ()
  "Return max point in *Messages* buffer."
  (logms-with-messages-buffer (point-max)))

(defun logms--last-call-stack-backtrace ()
  "Return the last stack frame right before of the logms function begin called.

By using this function to find the where the log came from.

It returns cons cell from by (current frame . backtrace)."
  (let* ((frames (backtrace-get-frames 'logms))
         (backtrace (list (nth 0 frames)))  ; always has the base frame
         (index 1) break frame evald)
    (while (not break)
      (setq frame (nth index frames)
            evald (backtrace-frame-evald frame)
            index (1+ index))
      (push frame backtrace)
      (when evald (setq break t)))
    ;; FRAME is the up one level call stack.
    (cons frame (reverse backtrace))))

(defun logms--find-logms-point (frame backtrace start args)
  "Move to the source point.

Argument FRAME is the deepest nearest frame.
Argument BACKTRACE is used to find the accurate position of the message.
Argument START to prevent search from the beginning of the file.

See function `logms--find-source' description for argument ARGS."
  ;; BACKTRACE will always return a list with minimum length of 1
  (let ((level (1- (length backtrace))) frame-args
        (end (save-excursion (forward-sexp) (point))) found (searching t)
        key val (count 0))
    (while (not found)
      (setq searching (re-search-forward logms--search-context end t))
      (unless searching    ; If search failed, start from the beginning, this
        (goto-char start)  ; occures when inside a loop
        (setq searching (re-search-forward logms--search-context end t)))
      (forward-char -1)  ; escape from string character
      (unless (logms--inside-comment-or-string-p)  ; comment or string?
        (when (= level (logms--nest-level-at-point))  ; compare frame level
          (setq frame-args (backtrace-frame-args (nth 0 backtrace)))
          (when (equal args frame-args)  ; compare arguments
            (setq key (cons args level) val (ht-get logms--log-map key))
            (setq count (1+ count))
            (when (or (null val) (< val count))
              (setq found t)
              (ht-set logms--log-map key count)))))
      ;; Revert last search point
      (goto-char searching))
    ;; Go back to the start of the symbol so it looks nicer
    (when found (re-search-backward logms--search-context start t))
    (point)))

(defun logms--make-button (beg end source pt)
  "Make a button from BEG to END.
Argument SOURCE is the buffer prints the log.
Argument PT indicates where the log beging print inside SOURCE buffer."
  (ignore-errors
    (make-text-button beg end 'follow-link t
                      'action (lambda (&rest _)
                                ;; If source is string, then it has to be a file path
                                (when (and (stringp source) (file-exists-p source))
                                  (save-window-excursion  ; find it, and update source
                                    (find-file source)
                                    (setq source (current-buffer))))
                                ;; Display the source buffer and it's position
                                (if (not (buffer-live-p source))
                                    (user-error "Buffer no longer exists: %s" source)
                                  (switch-to-buffer-other-window source)
                                  (goto-char pt))))))

(defun logms--find-source (call args)
  "Return the source information by CALL.

Argument ARGS is a list with format and printing arguments to compare and
to define the unique log."
  (save-excursion
    (let* ((source (current-buffer)) (pt (point))
           (line (line-number-at-pos pt))
           (column (current-column))
           (frame (car call)) (fnc (backtrace-frame-fun frame))
           (backtrace (cdr call))
           (old-buf-lst (buffer-list))
           find-function-after-hook found)
      (when (symbolp fnc)  ; If not symbol, it's evaluate from buffer
        (save-window-excursion
          (add-hook 'find-function-after-hook (lambda () (setq found t)))
          (let ((message-log-max nil) (inhibit-message t))
            (ignore-errors (find-function fnc)))
          ;; This return nil if success, so we use `unless' instead of `when'
          (when found
            ;; Update source information
            (setq source (buffer-file-name)
                  pt (logms--find-logms-point frame backtrace (point) args)
                  line (line-number-at-pos (point))
                  column (current-column))
            ;; Kill if it wasn't opened
            (unless (= (length old-buf-lst) (length (buffer-list)))
              (kill-buffer (current-buffer))))))
      (list source pt line column))))

;;;###autoload
(defun logms (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (if (not logms-show) (apply 'message fmt args)
    (add-hook 'post-command-hook #'logms--post-command)
    (let* ((call (logms--last-call-stack-backtrace))
           (info (logms--find-source call (append (list fmt) args)))
           (source (nth 0 info)) (pt (nth 1 info)) (line (nth 2 info)) (column (nth 3 info))
           (name (if (stringp source) (f-filename source) (buffer-name source)))
           (display (format "%s:%s:%s" name line column))
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
          (logms--make-button beg (+ beg display-len) source pt))))))

(defun logms--post-command ()
  "Post command hook."
  (ht-clear logms--log-map)  ; clear it once after each command's execution
  ;; Remove hook, so we don't waste performance
  (remove-hook 'post-command-hook #'logms--post-command))

(provide 'logms)
;;; logms.el ends here
