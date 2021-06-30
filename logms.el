;;; logms.el --- See where the message came from  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-26 23:22:27

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: See where the message came from
;; Keyword:
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (f "0.20.0") (s "1.9.0") (ht "2.3"))
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

(require 'cl-lib)
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

(defcustom logms-guess t
  "If non-nil, try to guess the declaration from eval history."
  :type 'boolean
  :group 'logms)

(defvar logms--eval-history nil
  "Records of all evaluate buffers.

If the eval buffer exists, then it will not be on this list.")

(defconst logms--search-context "(logms[ \t\"]*"
  "Regular expression to search for logms calls.")

(defvar logms--log-map (ht-create)
  "Record the log situation between from last to current command.

This resolved printing the same log in same frame level by acutally counting
the program execution.")

(defvar logms--show-log nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun logms--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when logms--show-log (apply 'message fmt args)))

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
    (/ (+ left right) 2)))

;;
;; (@* "Core" )
;;

(defun logms--clean-eval-history ()
  "Clean up evaluate history."
  (delete-dups logms--eval-history)
  (setq logms--eval-history
        (cl-remove-if (lambda (buf) (or (not (buffer-live-p buf))
                                        (with-current-buffer buf (buffer-file-name))))
                      logms--eval-history)))

(defun logms--record-eval-history ()
  "Record history of evaluate buffer."
  (setq logms--eval-history (append logms--eval-history eval-buffer-list))
  (logms--clean-eval-history))

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
    (unless (symbolp (backtrace-frame-fun frame))
      (pop backtrace))
    ;; FRAME is the up one level call stack. BACKTRACE is used to compare
    ;; the frame level.
    (cons frame (reverse backtrace))))

(defun logms--find-logms-point (backtrace start args)
  "Move to the source point.

Argument BACKTRACE is used to find the accurate position of the message.
Argument START to prevent search from the beginning of the file.

See function `logms--find-source' description for argument ARGS."
  ;; BACKTRACE will always return a list with minimum length of 1
  (let ((level (1- (length backtrace))) nest-level frame-args
        (end (or (ignore-errors (save-excursion (forward-sexp) (point)))
                 (point-max)))
        found (searching t)
        key val (count 0) missing)
    (while (and (not found) (not missing))
      (setq searching (re-search-forward logms--search-context end t))
      ;; If search failed, start from the beginning, this
      (unless searching
        ;; After search a round, if not found then it's missing
        (unless found (setq missing t))
        (goto-char start)  ; occures when inside a loop
        (setq searching (re-search-forward logms--search-context end t)))
      (search-backward "(logms" start t)
      (logms--log "\f")
      (logms--log "0: %s %s" (point) end)
      (unless (logms--inside-comment-or-string-p)  ; comment or string?
        (setq nest-level (logms--nest-level-at-point))
        (logms--log "1: %s %s %s" level nest-level (point))
        (when (= level nest-level)  ; compare frame level
          ;; To get the true arguments, it stores inside the first item
          ;; of BACKTRACE frames
          (setq frame-args (backtrace-frame-args (nth 0 backtrace)))
          (logms--log "2: %s %s" args frame-args)
          (when (equal args frame-args)  ; compare arguments
            ;; NOTE: LEVEL is inaccurate, NEST-LEVEL should be correct
            (setq key (cons args nest-level) val (ht-get logms--log-map key)
                  count (1+ count))
            (when (or (null val) (< val count))
              (setq found t)
              (ht-set logms--log-map key count)))))
      ;; Revert last search point
      (when searching (goto-char searching)))
    ;; Go back to the start of the symbol so it looks nicer
    (when found (search-backward "(logms" start t))
    (if missing 'missing (point))))

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

(defun logms--guess-buffer (caller)
  "Return guessed buffer and it's point from CALLER."
  (let (guessed-buffer point)
    (cl-some (lambda (buf)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-min))
                   (setq point (search-forward (symbol-name caller) nil t))
                   (when point
                     (goto-char point)
                     (search-backward "(" nil t)
                     (setq guessed-buffer buf
                           point (point)))))
               guessed-buffer)
             logms--eval-history)
    (cons guessed-buffer point)))

(defun logms--find-source (call args)
  "Return the source information by CALL.

Argument ARGS is a list with format and printing arguments to compare and
to define the unique log."
  (save-excursion
    (let* ((source (current-buffer)) (pt (point))
           (line (line-number-at-pos pt))
           (column (current-column))
           (frame (car call)) (caller (backtrace-frame-fun frame))
           (backtrace (cdr call))
           (old-buf-lst (buffer-list))
           find-function-after-hook found
           guessed-info guessed-buffer guessed-point
           (c-inter (eq caller this-command)) start)

      (save-window-excursion
        (when (symbolp caller)  ; If not symbol, it's evaluate from buffer
          (add-hook 'find-function-after-hook (lambda () (setq found t)))
          (let ((message-log-max nil) (inhibit-message t))
            (ignore-errors (find-function caller))))

        (if found
            (setq source (buffer-file-name))  ; Update source information

          ;; Record the evaluate history
          (logms--record-eval-history)

          ;; guess from evaluate buffer history
          (if (and logms-guess (symbolp caller))
              (setq guessed-info (logms--guess-buffer caller)
                    guessed-buffer (car guessed-info)
                    guessed-point (cdr guessed-info))
            (backward-sexp))))

      (setq source (or guessed-buffer source))

      (with-current-buffer (if found (current-buffer) source)
        (setq start (point))
        (setq pt (logms--find-logms-point backtrace (or guessed-point start) args)
              line (line-number-at-pos (point))
              column (current-column)))

      (when (equal pt 'missing)
        (setq pt start)  ; revert
        (when c-inter (user-error "Source missing, caller: %s" caller)))

      (when found
        ;; Kill if it wasn't opened
        (unless (= (length old-buf-lst) (length (buffer-list)))
          (kill-buffer (current-buffer))))
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
