;;; sendai-demo.el --- Setup to demonstrate sendai -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/sendai-theme

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some simple setup to make an Emacs frame demonstrating the Sendai theme.

;;; Code:

;; Set up UI basics.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)

;; Set the frame size and window configuration.
(set-frame-width (selected-frame) 160)
(set-frame-height (selected-frame) 64)
(split-window-below)
(split-window-right)
(other-window 2)
(split-window-right)
(other-window -2)

(add-hook 'change-major-mode-hook #'elide-head)
(setq-default enable-local-variables nil)

(let* ((emacs-src (getenv "EMACS_SRC"))
       (files (list (expand-file-name "README.org")
                    (expand-file-name "sendai-theme.el")
                    (expand-file-name "src/menu.c" emacs-src)
                    (expand-file-name "autogen.sh" emacs-src))))
  (add-to-list 'custom-theme-load-path default-directory)
  (load-theme 'sendai t)

  (dolist (file files)
    (find-file file)
    (ignore-errors (scroll-up-command))
    (other-window 1))

  ;; Clear the message from the minibuffer.
  (message ""))

;;; sendai-demo.el ends here
