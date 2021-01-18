;;; test.el --- anzu2.el test -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'anzu2)

(ert-deftest case-fold-search ()
  "Detect case fold search"
  (let ((isearch-mode t))
    (should (eq (anzu2--case-fold-search "foo") isearch-case-fold-search)))
  (let ((isearch-mode nil)
        (case-fold-search t))
    ;; start with lower chacater then case-insensitive search
    (should (anzu2--case-fold-search "abcde"))
    ;; start with Upper chacater then case-sensitive search
    (should-not (anzu2--case-fold-search "ABCDE"))))

(ert-deftest convert-for-lax-whitespace ()
  "Replace white space for lax-whitespace"
  (let ((replace-lax-whitespace t))
    (should (string= (anzu2--convert-for-lax-whitespace "a b[" nil) "a\\s-+b\\[")))
  (let ((replace-regexp-lax-whitespace t))
    (should (string= (anzu2--convert-for-lax-whitespace "a b" t) "a\\s-+b"))))

;;; test.el ends here
