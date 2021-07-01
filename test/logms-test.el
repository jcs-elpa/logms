;;; logms-test.el --- logms tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jen-Chieh Shen

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords:

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

;; Tests for logms

;;; Code:

(require 'logms)
(require 'ert)
(require 'debug)

(defun logms-call (msg1)
  "Calling it from a function."
  (logms "%s" msg1))

(ert-deftest logms-test-plain ()
  (should (null (logms "logms-test-plain"))))

(ert-deftest logms-test-nested ()
  (should (null (logms-call "logms-test-nested"))))

(ert-deftest logms-test-when ()
  (should (null (when t (logms "logms-test-when")))))

(ert-deftest logms-test-progn ()
  (should (null (progn (logms "logms-test-progn")))))

(ert-deftest logms-test-if ()
  (should (null (if t (logms "logms-test-if")))))

(ert-deftest logms-test-if-else ()
  (should (null (if nil (progn) (logms "logms-test-if-else")))))

(ert-deftest logms-test-if-else-progn ()
  (should (null (if nil (progn) (progn (logms "logms-test-if-else-progn"))))))

(provide 'logms-test)
;;; logms-test.el ends here
