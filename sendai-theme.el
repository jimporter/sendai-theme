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

      (red-darker "#4f282a")
      (orange-darker "#4d3225")
      (yellow-darker "#4e462b")
      (green-darker "#2b402e")
      (cyan-darker "#204142")
      (blue-darker "#273b55")
      (violet-darker "#35334e")
      (magenta-darker "#462b3f")

      (red-dark "#792c32")
      (orange-dark "#7d452b")
      (yellow-dark "#796b2f")
      (green-dark "#2a5d31")
      (cyan-dark "#185d5f")
      (blue-dark "#2a527e")
      (violet-dark "#474881")
      (magenta-dark "#702e5e")

      (red-primary "#d33f4d")
      (orange-primary "#d47732")
      (yellow-primary "#ddb63a")
      (green-primary "#4ba33f")
      (cyan-primary "#26a19f")
      (blue-primary "#3491dc")
      (violet-primary "#8578db")
      (magenta-primary "#bc4d99")

      (red-light "#e97173")
      (orange-light "#ec9f63")
      (yellow-light "#ded692")
      (green-light "#7cc36e")
      (cyan-light "#3edbe6")
      (blue-light "#6cc2ff")
      (violet-light "#afa2f2")
      (magenta-light "#db7fbb"))

  (custom-theme-set-faces
   'sendai

   ;; ----------
   ;; Core faces
   ;; ----------

   ;; Basics
   `(default ((,class (:background ,bg-primary :foreground ,fg-primary))))
   `(cursor ((,class (:background ,fg-primary))))
   `(shadow ((,class (:foreground ,fg-darker))))
   `(link ((,class (:foreground ,blue-primary :underline t))))
   `(link-visited ((,class (:foreground ,violet-primary :underline t))))
   `(success ((,class (:foreground ,green-primary :weight bold))))
   `(warning ((,class (:foreground ,orange-primary :weight bold))))
   `(error ((,class (:foreground ,red-primary :weight bold))))
   `(escape-glyph ((,class (:foreground ,red-primary :weight bold))))
   `(homoglyph ((,class (:foreground ,cyan-light))))
   `(nobreak-hyphen ((,class (:foreground ,cyan-light))))

   ;; Highlighting
   `(fringe ((,class (:background ,bg-primary))))
   `(highlight ((,class (:background ,cyan-dark))))
   `(region ((,class (:background ,blue-dark :foreground ,fg-primary))))
   `(secondary-selection ((,class (:background ,bg-lighter))))
   `(match ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(isearch ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(isearch-fail ((,class (:background ,red-dark))))
   `(lazy-highlight
     ((,class (:background ,fg-darker :foreground ,bg-primary))))
   `(completions-common-part ((,class (:foreground ,blue-primary))))
   `(show-paren-match ((,class (:foreground ,cyan-light :weight bold))))
   `(show-paren-match-expression ((,class (:background ,blue-darker))))
   `(show-paren-mismatch
     ((,class (:background ,red-primary :foreground ,fg-light))))
   `(trailing-whitespace ((,class (:background ,red-primary))))

   ;; Mode line
   `(mode-line ((,class (:background ,bg-light :foreground ,fg-light))))
   `(mode-line-inactive ((,class (:background ,bg-dark :foreground ,fg-dark))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color ,fg-darker)))))

   ;; Misc UI
   `(minibuffer-prompt ((,class (:foreground ,blue-primary :weight bold))))
   `(header-line ((,class (:background ,bg-lighter :foreground ,fg-primary))))
   `(header-line-highlight
     ((,class (:background ,fg-darker :foreground ,bg-primary))))
   `(line-number ((,class (:foreground ,fg-darker))))
   `(line-number-current-line ((,class (:foreground ,fg-primary))))
   `(line-number-major-tick ((,class (:background ,bg-lighter))))
   `(line-number-minor-tick ((,class (:background ,bg-light))))
   `(widget-field ((,class (:background ,bg-lighter))))
   `(widget-single-line-field ((,class (:background ,bg-lighter))))
   `(vertical-border ((,class (:foreground ,bg-lighter))))
   `(window-divider ((,class (:foreground ,bg-lighter))))
   `(window-divider-first-pixel ((,class (:foreground ,fg-darker))))
   `(window-divider-last-pixel ((,class (:foreground ,bg-dark))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,blue-light))))
   `(font-lock-comment-face ((,class (:foreground ,fg-darker))))
   `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,class (:foreground ,cyan-primary))))
   `(font-lock-function-name-face
     ((,class (:foreground ,yellow-primary :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,blue-primary))))
   `(font-lock-negation-char-face ((,class (:foreground ,yellow-light))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow-light))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan-primary))))
   `(font-lock-string-face ((,class (:foreground ,green-light))))
   `(font-lock-type-face ((,class (:foreground ,cyan-light))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow-light))))
   `(font-lock-warning-face ((,class (:foreground ,red-primary))))

   ;; -----------------
   ;; Built-in packages
   ;; -----------------

   ;; calendar
   `(calendar-month-header
     ((,class (:foreground ,yellow-primary :weight bold))))
   `(calendar-weekday-header ((,class (:foreground ,blue-primary))))
   `(calendar-weekend-header ((,class (:foreground ,blue-light))))

   ;; compilation-mode
   `(compilation-warning ((,class (:foreground ,yellow-primary :weight bold))))
   `(compilation-line-number ((,class (:foreground ,fg-darker))))
   `(compilation-column-number ((,class (:foreground ,fg-darker))))
   `(compilation-mode-line-exit
     ((,class (:foreground ,green-primary :weight bold))))
   `(compilation-mode-line-fail
     ((,class (:foreground ,red-primary :weight bold))))
   `(compilation-mode-line-run
     ((,class (:foreground ,yellow-primary :weight bold))))

   ;; customize
   `(custom-group-tag
     ((,class (:foreground ,yellow-primary :weight bold :height 1.2))))
   `(custom-group-tag-1
     ((,class (:foreground ,cyan-light :weight bold :height 1.2))))
   `(custom-state ((,class (:foreground ,green-primary))))
   `(custom-changed ((,class (:foreground ,yellow-light))))
   `(custom-modified ((,class (:foreground ,yellow-light))))
   `(custom-themed ((,class (:foreground ,yellow-light))))
   `(custom-set ((,class (:background ,green-light :foreground ,bg-primary))))
   `(custom-invalid ((,class (
      :foreground ,red-primary :weight bold :underline (:style wave)))))
   `(custom-rogue ((,class (
      :foreground ,orange-primary :weight bold :underline (:style wave)))))
   `(custom-variable-tag ((,class (:foreground ,blue-light :weight bold))))
   `(custom-variable-obsolete ((,class (:foreground ,fg-darker :weight bold))))
   `(custom-button ((,class (
      :background ,fg-primary :foreground ,bg-primary
      :box (:line-width 1 :style released-button)))))
   `(custom-button-mouse ((,class (
      :background ,fg-light :foreground ,bg-primary
      :box (:line-width 1 :style released-button)))))
   `(custom-button-pressed ((,class (
      :background ,fg-primary :foreground ,bg-primary
      :box (:line-width 1 :style pressed-button)))))
   `(custom-button-unraised ((,class (:inherit default :underline t))))
   `(custom-button-pressed-unraised
     ((,class (:inherit custom-button-unraised :foreground ,violet-primary))))

   ;; diff-mode
   `(diff-header ((,class (:foreground ,fg-primary))))
   `(diff-file-header ((,class (:foreground ,yellow-primary :weight bold))))
   `(diff-hunk-header ((,class (:foreground ,cyan-light))))
   `(diff-added ((,class (:background ,green-darker))))
   `(diff-removed ((,class (:background ,red-darker))))
   `(diff-changed ((,class (:background ,yellow-darker))))
   `(diff-indicator-added
     ((,class (:foreground ,green-light :inherit diff-added))))
   `(diff-indicator-removed
     ((,class (:foreground ,red-light :inherit diff-removed))))
   `(diff-indicator-changed
     ((,class (:foreground ,yellow-light :inherit diff-changed))))
   `(diff-refine-added ((,class (:background ,green-dark))))
   `(diff-refine-removed ((,class (:background ,red-dark))))
   `(diff-refine-changed ((,class (:background ,yellow-dark))))

   ;; dired
   `(dired-header ((,class (:foreground ,green-light :weight bold))))
   `(dired-directory ((,class (:foreground ,blue-primary :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan-light))))
   `(dired-special ((,class (:foreground ,yellow-light))))
   `(dired-marked ((,class (:foreground ,yellow-primary :weight bold))))
   `(dired-mark ((,class (:inherit dired-marked))))
   `(dired-flagged ((,class (:foreground ,red-primary :weight bold))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,fg-darker :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,blue-primary :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,magenta-primary :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,green-light))))
   `(eshell-ls-product ((,class (:foreground ,cyan-primary))))
   `(eshell-ls-readonly ((,class (:foreground ,orange-light))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan-light))))
   `(eshell-ls-special ((,class (:foreground ,yellow-light))))
   `(eshell-ls-backup ((,class (:foreground ,fg-darker))))
   `(eshell-ls-missing ((,class (:foreground ,red-primary :weight bold))))
   `(eshell-ls-clutter ((,class (:foreground ,red-primary :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,orange-primary))))

   ;; flyspell
   `(flyspell-duplicate
     ((,class (:underline (:color ,orange-primary :style wave)))))
   `(flyspell-incorrect
     ((,class (:underline (:color ,red-primary :style wave)))))

   ;; gdb
   `(breakpoint-enabled ((,class (:foreground ,red-primary))))
   `(breakpoint-disabled ((,class (:foreground ,fg-darker))))

   ;; info-mode
   `(info-title-1 ((,class (:height 1.5 :weight bold))))
   `(info-title-2 ((,class (:height 1.4 :weight bold))))
   `(info-title-3 ((,class (:height 1.3 :weight bold))))
   `(info-title-4 ((,class (:height 1.1 :weight bold))))
   `(info-menu-header ((,class (:weight bold))))
   `(info-menu-star ((,class (:foreground ,yellow-primary))))
   `(info-node ((,class (:foreground ,fg-light :weight bold :slant italic))))

   ;; org-mode
   ;; Note: `org-level-N' is inherited from `outline-N'.
   `(org-document-info-keyword ((,class (:inherit org-meta-line))))
   `(org-document-title
     ((,class (:foreground ,blue-light :weight bold))))
   `(org-document-info ((,class (:foreground ,blue-light))))
   `(org-hide ((,class (:foreground ,bg-primary))))
   `(org-checkbox ((,class (:foreground ,fg-darker :weight bold))))
   `(org-latex-and-related ((,class (:foreground ,fg-darker))))
   `(org-footnote ((,class (:foreground ,blue-primary :underline t))))
   `(org-date ((,class (:foreground ,cyan-primary :underline t))))
   `(org-sexp-date ((,class (:foreground ,cyan-primary))))
   `(org-todo ((,class (:foreground ,red-light :weight bold))))
   `(org-done ((,class (:foreground ,green-light :weight bold))))
   `(org-tag ((,class (:foreground ,yellow-light :weight normal))))
   `(org-tag-group
     ((,class (:foreground ,yellow-light :weight normal :slant italic))))
   `(org-priority ((,class (:foreground ,orange-light))))
   `(org-macro ((,class (:foreground ,magenta-light))))
   `(org-block
     ((,class (:background ,blue-darker :foreground ,fg-primary :extend t))))
   `(org-quote ((,class (:slant italic))))
   `(org-verse ((,class (:slant italic))))
   `(org-table ((,class (:background ,blue-darker :foreground ,blue-light))))
   `(org-drawer ((,class (:foreground ,blue-light))))
   `(org-special-keyword ((,class (:foreground ,blue-primary))))
   `(org-date-selected
     ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(org-agenda-structure ((,class (:foreground ,fg-darker))))
   `(org-agenda-date ((,class (:foreground ,blue-primary))))
   `(org-agenda-date-weekend ((,class (:foreground ,blue-light))))
   `(org-agenda-date-today ((,class (:foreground ,yellow-light))))
   `(org-time-grid ((,class (:foreground ,fg-darker))))
   `(org-agenda-current-time ((,class (:foreground ,yellow-light))))
   `(org-agenda-done ((,class (:foreground ,violet-primary))))
   `(org-scheduled ((,class (:foreground ,violet-light))))
   `(org-scheduled-previously ((,class (:foreground ,orange-light))))
   `(org-scheduled-today ((,class (:foreground ,violet-light))))

   ;; outline-mode
   `(outline-1 ((,class (:foreground ,yellow-primary :weight bold))))
   `(outline-2 ((,class (:foreground ,cyan-light :weight bold))))
   `(outline-3 ((,class (:foreground ,violet-light :weight bold))))
   `(outline-4 ((,class (:foreground ,blue-light :weight bold))))
   `(outline-5 ((,class (:foreground ,yellow-light :weight bold))))
   `(outline-6 ((,class (:foreground ,cyan-light :weight bold))))
   `(outline-7 ((,class (:foreground ,violet-light :weight bold))))
   `(outline-8 ((,class (:foreground ,blue-light :weight bold))))

   ;; sh-mode
   `(sh-escaped-newline ((,class (:foreground ,fg-darker))))
   `(sh-heredoc ((,class (:foreground ,green-light))))
   `(sh-quoted-exec ((,class (:foreground ,orange-light))))

   ;; tab-bar
   `(tab-bar ((,class (:background ,bg-lighter :inherit variable-pitch))))
   `(tab-bar-tab ((,class
      (:background ,bg-primary :foreground ,fg-primary :box ,bg-primary))))
   `(tab-bar-tab-inactive ((,class
      (:background ,bg-dark :foreground ,fg-darker :box ,bg-dark))))

   ;; tab-line
   `(tab-line ((,class (:background ,bg-lighter :inherit variable-pitch))))
   `(tab-line-tab ((,class
      (:background ,bg-primary :foreground ,fg-darker :box ,bg-primary))))
   `(tab-line-tab-current ((,class
      (:background ,bg-primary :foreground ,fg-primary :box ,bg-primary))))
   `(tab-line-tab-inactive ((,class
      (:background ,bg-dark :foreground ,fg-darker :box ,bg-dark))))

   ;; term
   `(term-color-black
     ((,class (:background ,bg-lighter :foreground ,bg-lighter))))
   `(term-color-red
     ((,class (:background ,red-light :foreground ,red-light))))
   `(term-color-green
     ((,class (:background ,green-light :foreground ,green-light))))
   `(term-color-yellow
     ((,class (:background ,yellow-light :foreground ,yellow-light))))
   `(term-color-blue
     ((,class (:background ,blue-primary :foreground ,blue-primary))))
   `(term-color-magenta
     ((,class (:background ,magenta-primary :foreground ,magenta-primary))))
   `(term-color-cyan
     ((,class (:background ,cyan-primary :foreground ,cyan-primary))))
   `(term-color-white
     ((,class (:background ,fg-primary :foreground ,fg-primary))))

   ;; --------------------
   ;; Third-party packages
   ;; --------------------

   ;; hl-todo
   `(hl-todo ((,class (:foreground ,yellow-light :weight bold))))

   ;; js2-mode
   `(js2-external-variable ((,class (:foreground ,orange-primary))))
   `(js2-function-param ((,class (:foreground ,green-primary))))
   `(js2-jsdoc-tag ((,class (:inherit c-annotation-face))))
   `(js2-jsdoc-type ((,class (:inherit font-lock-type-face))))
   `(js2-jsdoc-value ((,class (:inherit font-lock-variable-name-face))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,fg-primary))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,yellow-primary))))
   `(js2-error ((,class (:foreground ,red-primary))))
   `(js2-warning ((,class (:underline (:color ,orange-primary :style wave)))))

   ;; markdown-mode
   `(markdown-html-tag-delimiter-face ((,class (:inherit default))))
   `(markdown-html-tag-name-face
     ((,class (:inherit font-lock-function-name-face))))
   `(markdown-html-attr-name-face
     ((,class (:inherit font-lock-variable-name-face))))
   `(markdown-html-attr-value-face ((,class (:inherit font-lock-string-face))))
   `(markdown-html-entity-face
     ((,class (:inherit font-lock-variable-name-face))))
   `(markdown-gfm-checkbox-face
     ((,class (:foreground ,fg-darker :weight bold))))
   `(markdown-table-face
     ((,class (:foreground ,blue-light :background ,blue-darker))))
   `(markdown-inline-code-face ((,class (:foreground ,fg-darker))))
   `(markdown-pre-face ((,class (:foreground ,fg-darker))))
   `(markdown-code-face ((,class (:background ,blue-darker :extend t))))
   `(markdown-language-keyword-face ((,class (:foreground ,cyan-light))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,fg-primary))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,fg-darker))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,fg-light))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,fg-dark))))
   `(rainbow-delimiters-base-error-face ((,class (:foreground ,red-primary))))

   ;; which-key
   `(which-key-key-face ((,class (:foreground ,yellow-primary))))
   `(which-key-command-description-face ((,class (:foreground ,blue-light))))
   `(which-key-group-description-face
     ((,class (:foreground ,cyan-primary :weight bold))))

   ;; yaml-mode
   `(yaml-tab-face ((,class (:background ,red-primary))))
   )

  (custom-theme-set-variables
   'sendai
   `(ansi-color-names-vector
     [,bg-lighter ,red-light ,green-light ,yellow-light ,blue-primary
                  ,magenta-primary ,cyan-primary ,fg-primary])

   `(hl-todo-keyword-faces '(("FIXME" . ,red-primary)
                             ("TODO"  . ,orange-primary)
                             ("XXX"   . ,orange-primary)))

   `(rainbow-delimiters-max-face-count 4)

   `(xterm-color-names
     [,bg-lighter ,red-light ,green-light ,yellow-light ,blue-primary
                  ,magenta-primary ,cyan-primary ,fg-primary])
   `(xterm-color-names-bright
     [,fg-darker ,red-primary ,green-primary ,yellow-primary ,blue-light
                 ,magenta-light ,cyan-light ,fg-light])))

(provide-theme 'sendai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; sendai-theme.el ends here
