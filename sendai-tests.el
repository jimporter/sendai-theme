;;; sendai-tests.el --- Tests for sendai -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Jim Porter

;; Author: Jim Porter
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the Sendai theme

;;; Code:

(require 'ert)

;;; Tests:

(ert-deftest sendai-tests/subst-p/basic ()
  "Check that `sendai--subst-p' returns nil for basic types."
  (should-not (sendai--subst-p "foobar"))
  (should-not (sendai--subst-p 'foobar))
  (should-not (sendai--subst-p :foobar))
  (should-not (sendai--subst-p 42)))

(ert-deftest sendai-tests/subst-p/color ()
  "Check that `sendai--subst-p' returns non-nil for color objects."
  (should (sendai--subst-p (sendai--color "red" "blue"))))

(ert-deftest sendai-tests/subst-p/sequence ()
  "Check that `sendai--subst-p' works for various sequence types."
  (should-not (sendai--subst-p '(1 2 3)))
  (should-not (sendai--subst-p '(1 . 2)))
  (should-not (sendai--subst-p [1 2 3]))
  (should-not (sendai--subst-p '("foo" "bar" "baz")))
  (should-not (sendai--subst-p '("foo" . "bar")))
  (should-not (sendai--subst-p ["foo" "bar" "baz"]))
  (let ((color (sendai--color "red" "blue")))
    (should (sendai--subst-p (list color 1 2)))
    (should (sendai--subst-p (cons color 1)))
    (should (sendai--subst-p (vector color 1 2)))
    (should (sendai--subst-p (list 1 2 color)))
    (should (sendai--subst-p (vector 1 2 color)))))

;;; sendai-tests.el ends here
