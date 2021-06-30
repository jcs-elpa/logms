;;; logms.el --- See where the message came from  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-26 23:22:27

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: See where the message came from
;; Keyword:
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4") (s "1.9.0"))
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
(require 's)

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

;;
;; (@* "Util" )
;;

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
    (/ (+ left right) 2)))

(defun logms--return-args-at-point ()
  "Return the full argument from point."
  (let ((beg (point)) content)
    (save-excursion
      (forward-sexp)
      (setq content (buffer-substring beg (point))))
    (setq content (s-replace "(" "" content)
          content (s-replace ")" "" content)
          content (s-replace-regexp "logms[ ]*" "" content))
    (split-string content "\"" t)))

(defun logms--compare-list (lst1 lst2)
  "Return non-nil if LST1 is identical with LST2."
  (let ((index 0) (len1 (length lst1)) (len2 (length lst2)) (same t) break
        item1 item2)
    (unless (= len1 len2) (setq same nil))
    (while (and same (not break) (< index len1))
      (setq item1 (nth index lst1) item2 (nth index lst2)
            item1 (format "%s" item1) item2 (format "%s" item2)
            ;; Trim the argument string should be fine since we are comparing
            ;; arguments and not the string itself!
            item1 (string-trim item1) item2 (string-trim item2))
      (unless (string= item1 item2) (setq same nil break t))
      (setq index (1+ index)))
    same))

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
  (let (backtrace (index 0) frame break exec meet flag)
    (while (not break)
      (setq frame (backtrace-frame index)
            flag (nth 0 frame) exec (nth 1 frame)
            index (1+ index))
      ;; Find the next call stack, if flag returns non-symbol then
      ;; it is call from the beginning of the call stack
      ;;
      ;; Otherwise, we push all local frames
      (when meet (if flag (setq break t) (push frame backtrace)))
      ;; First we find the logms call stack
      (when (eq exec 'logms) (setq meet t)))
    (cons frame (reverse backtrace))))

(defun logms--find-logms-point (backstrace start args)
  "Move to the source point.

Argument START to prevent search from the beginning of the file.
Argument BACKSTRACE is used to find the accurate position of the message.

See function `logms--find-source' description for argument ARGS."
  (let ((level (length backstrace)) parsed-args
        (end (save-excursion (forward-sexp) (point))) found (searching t))
    (when (= level 0) (setq level 1))
    (while (and (not found) searching)
      (setq searching (re-search-forward logms--search-context end t))
      (re-search-backward "(" start t)
      (when (= level (logms--nest-level-at-point))
        (setq parsed-args (logms--return-args-at-point))
        (when (logms--compare-list args parsed-args)
          ;; FIXME: If user has same log string in same level then this
          ;; will gave them the same result
          (setq found t)))
      (when searching (goto-char searching)))
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
           (frame (car call)) (fnc (nth 1 frame))
           (backstrace (cdr call))
           (old-buf-lst (buffer-list))
           find-function-after-hook found)
      (when (symbolp fnc)  ; If not symbol, it's evaluate from buffer
        (save-window-excursion
          (add-hook 'find-function-after-hook (lambda () (setq found t)))
          (ignore-errors (find-function fnc))
          ;; This return nil if success, so we use `unless' instead of `when'
          (when found
            ;; Update source information
            (setq source (current-buffer)
                  pt (logms--find-logms-point backstrace (point) args)
                  line (line-number-at-pos (point))
                  column (current-column))
            ;; Kill if it wasn't opened
            (unless (= (length old-buf-lst) (length (buffer-list)))
              (kill-buffer (current-buffer))))))
      (list source pt line column))))

;;;###autoload
(defun logms (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (if logms-show
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
            (logms--make-button beg (+ beg display-len) source pt))))
    (apply 'message fmt args)))

(provide 'logms)
;;; logms.el ends here
