;;; sendai-theme.el --- A cool blue color theme

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/sendai-theme
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((emacs "24"))

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

;; A cool blue theme for Emacs, aiming for medium levels of contrast to
;; maintain readability without straining your eyes with garish colors.

;;; Code:

(deftheme sendai "A cool blue color theme")

(let ((class '((class color) (min-colors 89)))
      (bg-dark "#181e25")
      (bg-primary "#232c38")
      (bg-light "#364454")
      (bg-lighter "#4a5b71")

      (fg-darker "#7b96b7")
      (fg-dark "#a2b6d0")
      (fg-primary "#c9d6e9")
      (fg-light "#f0f6fe")

      (red "#d33f4d")
      (orange "#da7341")
      (yellow "#ddb63a")
      (green "#4ba33f")
      (cyan "#26a19f")
      (blue "#3491dc")
      (violet "#8578db")
      (magenta "#bc4d99")

      (red-dark "#862c35")
      (orange-dark "#834732")
      (yellow-dark "#7a6c30")
      (green-dark "#316438")
      (cyan-dark "#1e6264")
      (blue-dark "#2a527e")
      (violet-dark "#494a83")
      (magenta-dark "#763363")

      (red-light "#de6d6e")
      (orange-light "#ec9f63")
      (yellow-light "#e0d591")
      (green-light "#7cc36e")
      (cyan-light "#3edbe6")
      (blue-light "#6cc2ff")
      (violet-light "#afa0fe")
      (magenta-light "#db7fbb"))

  (custom-theme-set-faces
   'sendai
   `(default ((,class (:background ,bg-primary :foreground ,fg-primary))))
   `(shadow ((,class (:foreground ,fg-darker))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,violet :underline t))))

   `(success ((,class (:foreground ,green :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(error ((,class (:foreground ,red :weight bold))))

   `(escape-glyph ((,class (:foreground ,red :weight bold))))
   `(cursor ((,class (:background ,fg-primary))))

   ; Highlighting
   `(fringe ((,class (:background ,bg-primary))))
   `(highlight ((,class (:background ,cyan-dark))))
   `(region ((,class (:background ,blue-dark :foreground ,fg-primary))))
   `(secondary-selection ((,class (:background ,bg-lighter))))
   `(isearch ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(isearch-fail ((,class (:background ,red-dark))))
   `(lazy-highlight ((,class (:background ,bg-lighter))))
   `(show-paren-match ((,class (:background ,cyan :foreground ,fg-light))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,fg-light))))
   `(trailing-whitespace ((,class (:background ,red))))

   ; Modeline
   `(mode-line ((,class (:background ,bg-light :foreground ,fg-light))))
   `(mode-line-inactive ((,class (:background ,bg-dark :foreground ,fg-dark))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color ,fg-darker)))))

   ; Customize
   `(custom-button ((,class (
      :background ,fg-primary :foreground ,bg-primary
      :box (:line-width 1 :style released-button)))))
   `(custom-button-mouse ((,class (
      :background ,fg-light :foreground ,bg-primary
      :box (:line-width 1 :style released-button)))))
   `(custom-button-pressed ((,class (
      :background ,fg-primary :foreground ,bg-primary
      :box (:line-width 1 :style pressed-button)))))
   `(custom-state ((,class (:foreground ,green))))
   `(custom-invalid
     ((,class (:foreground ,red :weight bold :underline (:style wave)))))
   `(custom-variable-tag ((,class (:foreground ,blue-light :weight bold))))

   ; Misc UI
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))
   `(header-line ((,class (:background ,bg-lighter :foreground ,fg-primary))))
   `(header-line-highlight
     ((,class (:background ,fg-darker :foreground ,bg-primary))))
   `(line-number ((,class (:foreground ,fg-darker))))
   `(line-number-current-line ((,class (:foreground ,fg-primary))))
   `(widget-field ((,class (:background ,bg-lighter))))
   `(widget-single-line-field ((,class (:background ,bg-lighter))))

   ; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,blue-light))))
   `(font-lock-comment-face ((,class (:foreground ,fg-darker))))
   `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,blue :weight normal))))
   `(font-lock-negation-char-face ((,class (:foreground ,yellow-light))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow-light))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan))))
   `(font-lock-string-face ((,class (:foreground ,green-light))))
   `(font-lock-type-face ((,class (:foreground ,cyan-light))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow-light))))
   `(font-lock-warning-face ((,class (:foreground ,red))))

   ; eshell
   `(eshell-prompt ((,class (:foreground ,fg-darker :weight bold))))

   ; gdb
   `(breakpoint-enabled ((,class (:foreground ,red))))
   `(breakpoint-disabled ((,class (:foreground ,fg-darker))))

   ; diff-mode

   ; org-mode
   `(org-level-1 ((,class (:foreground ,yellow :weight bold))))
   `(org-level-2 ((,class (:foreground ,green :weight bold))))
   `(org-level-3 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-4 ((,class (:foreground ,blue :weight bold))))
   `(org-level-5 ((,class (:foreground ,yellow-light :weight bold))))
   `(org-level-6 ((,class (:foreground ,green-light :weight bold))))
   `(org-level-7 ((,class (:foreground ,cyan-light :weight bold))))
   `(org-level-8 ((,class (:foreground ,blue-light :weight bold))))
   `(org-todo ((,class (:foreground ,red-light :weight bold))))
   `(org-done ((,class (:foreground ,green-light :weight bold))))

   ; which-key
   `(which-key-key-face ((,class (:foreground ,yellow))))
   `(which-key-command-description-face ((,class (:foreground ,blue-light))))
   `(which-key-group-description-face
     ((,class (:foreground ,cyan :weight bold))))
   ))

(provide-theme 'sendai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; sendai-theme.el ends here
