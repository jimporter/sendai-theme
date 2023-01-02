;;; sendai-tests.el --- Tests for sendai -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Jim Porter

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

(ert-deftest sendai-tests/subst-p/nested ()
  "Check that `sendai--subst-p' works for nested sequence types."
  (should-not (sendai--subst-p (list '(1 2 3) [4 5 6])))
  (let ((color (sendai--color "red" "blue")))
    (should (sendai--subst-p (list (list color 1 2) [4 5 6])))
    (should (sendai--subst-p (list '(1 2 3) (vector color 5 6))))))

(ert-deftest sendai-tests/subst/constant ()
  "Check that `sendai-subst' doesn't do anything to constants."
  (should (equal (sendai-subst '(:foreground "red") 'true-color)
                 '(:foreground "red")))
  (should (equal (sendai-subst '(:foreground "red") '256-color)
                 '(:foreground "red")))
  (should (equal (sendai-subst '(:foreground "red") 'tty-color)
                 '(:foreground "red"))))

(ert-deftest sendai-tests/subst/color ()
  "Check that `sendai-subst' applies the correct color spec."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal (sendai-subst `(:foreground ,color) 'true-color)
                   '(:foreground "red")))
    (should (equal (sendai-subst `(:foreground ,color) '256-color)
                   '(:foreground "blue")))
    (should (equal (sendai-subst `(:foreground ,color) 'tty-color)
                   '(:foreground "green")))))

(ert-deftest sendai-tests/subst/color-nested ()
  "Check that `sendai-subst' applies the correct color spec to nested forms."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal (sendai-subst `(:underline (:color ,color :style wave))
                                 'true-color)
                   '(:underline (:color "red" :style wave))))
    (should (equal (sendai-subst `(:underline (:color ,color :style wave))
                                 '256-color)
                   '(:underline (:color "blue" :style wave))))
    (should (equal (sendai-subst `(:underline (:color ,color :style wave))
                                 'tty-color)
                   '(:underline (:color "green" :style wave))))))

(ert-deftest sendai-tests/make-face/constant ()
  "Check that `sendai-make-face' doesn't do anything to constants."
  (should (equal (sendai-make-face '(256-color) nil :foreground "red")
                 '((default :foreground "red"))))
  (should (equal (sendai-make-face '(256-color) nil
                                   :box '(:line-width (1 . -1) :color "blue"))
                 '((default :box (:line-width (1 . -1) :color "blue"))))))

(ert-deftest sendai-tests/make-face/color ()
  "Check that `sendai-make-face' applies the correct color spec."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-make-face '(256-color) nil :foreground color)
             '((((class color) (min-colors 256)) :foreground "blue"))))
    (should (equal
             (sendai-make-face '(256-color) nil
                               :box `(:line-width (1 . -1) :color ,color))
             '((((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/make-face/mixed ()
  "Check that `sendai-make-face' properly splits constant and variable specs."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-make-face '(256-color) nil
                               :weight 'bold
                               :box `(:line-width (1 . -1) :color ,color))
             '((default :weight bold)
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/make-face/extra ()
  "Check that `sendai-make-face' applies the correct color spec."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-make-face '(256-color) '((type graphic))
                               :weight 'bold
                               :box `(:line-width (1 . -1) :color ,color))
             '((default :weight bold)
               (((type graphic) (class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/face/constant ()
  "Check that `sendai-face' doesn't do anything to constants."
  (should (equal
           (sendai-face :foreground "red")
           '((default :foreground "red"))))
  (should (equal
           (sendai-face :box '(:line-width (1 . -1) :color "blue"))
           '((default :box (:line-width (1 . -1) :color "blue"))))))

(ert-deftest sendai-tests/face/color ()
  "Check that `sendai-face' applies the correct color spec."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-face :foreground color)
             '((((class color) (min-colors 16777216)) :foreground "red")
               (((class color) (min-colors 256)) :foreground "blue"))))
    (should (equal
             (sendai-face :box `(:line-width (1 . -1) :color ,color))
             '((((class color) (min-colors 16777216))
                :box (:line-width (1 . -1) :color "red"))
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/face/mixed ()
  "Check that `sendai-face' properly splits constant and variable specs."
  (let ((color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-face :weight 'bold
                          :box `(:line-width (1 . -1) :color ,color))
             '((default :weight bold)
               (((class color) (min-colors 16777216))
                :box (:line-width (1 . -1) :color "red"))
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/face/color/inherit-tty ()
  "Check that `sendai-face' applies the correct color spec."
  (let ((sendai-theme-inherit-tty-colors t)
        (color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-face :foreground color)
             '((((class color) (min-colors 16777216)) :foreground "red")
               (((class color) (min-colors 256) (type tty))
                :foreground "green")
               (((class color) (min-colors 256)) :foreground "blue"))))
    (should (equal
             (sendai-face :box `(:line-width (1 . -1) :color ,color))
             '((((class color) (min-colors 16777216))
                :box (:line-width (1 . -1) :color "red"))
               (((class color) (min-colors 256) (type tty))
                :box (:line-width (1 . -1) :color "green"))
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/face/mixed/inherit-tty ()
  "Check that `sendai-face' properly splits constant and variable specs."
  (let ((sendai-theme-inherit-tty-colors t)
        (color (sendai--color "red" "blue" "green")))
    (should (equal
             (sendai-face :weight 'bold
                          :box `(:line-width (1 . -1) :color ,color))
             '((default :weight bold)
               (((class color) (min-colors 16777216))
                :box (:line-width (1 . -1) :color "red"))
               (((class color) (min-colors 256) (type tty))
                :box (:line-width (1 . -1) :color "green"))
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "blue")))))))

(ert-deftest sendai-tests/let-palette ()
  "Check that `sendai-let-palette' works."
  (sendai-let-palette
    (should (equal
             (sendai-face :weight 'bold
                          :box `(:line-width (1 . -1) :color ,red-primary))
             '((default :weight bold)
               (((class color) (min-colors 16777216))
                :box (:line-width (1 . -1) :color "#d33f4d"))
               (((class color) (min-colors 256))
                :box (:line-width (1 . -1) :color "#d70000")))))))

;;; sendai-tests.el ends here
