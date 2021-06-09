;;; sendai-theme.el --- A cool blue color theme -*- lexical-binding: t -*-

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

(defgroup sendai-theme nil
  "Sendai-theme options."
  :group 'faces)

(defcustom sendai-theme-inherit-tty-colors nil
  "Use basic TTY colors when in xterm-256color mode."
  :type 'boolean
  :group 'sendai-theme)

(defun sendai--palettize (true-color xterm256-color &optional tty-color)
  (if (or (display-graphic-p) (= (tty-display-color-cells) 16777216))
      true-color
    (if (and sendai-theme-inherit-tty-colors tty-color)
        tty-color xterm256-color)))

(let ((class '((class color) (min-colors 89)))
      (bg-dark         (sendai--palettize "#181e25" "#1c1c1c"))
      (bg-primary      (sendai--palettize "#232c38" "#303030"))
      (bg-light        (sendai--palettize "#364454" "#444444"))
      (bg-lighter      (sendai--palettize "#4a5b71" "#5f5f5f" "black"))

      (fg-darker       (sendai--palettize "#7b96b7" "#5f87af" "brightblack"))
      (fg-dark         (sendai--palettize "#a2b6d0" "#87afd7"))
      (fg-primary      (sendai--palettize "#c9d6e9" "#d7d7d7" "white"))
      (fg-light        (sendai--palettize "#f0f6fe" "#ffffff" "brightwhite"))

      (red-darker      (sendai--palettize "#4f282a" nil))
      (orange-darker   (sendai--palettize "#4d3225" nil))
      (yellow-darker   (sendai--palettize "#4e462b" nil))
      (green-darker    (sendai--palettize "#2b402e" nil))
      (cyan-darker     (sendai--palettize "#204142" nil))
      (blue-darker     (sendai--palettize "#273b55" nil))
      (violet-darker   (sendai--palettize "#35334e" nil))
      (magenta-darker  (sendai--palettize "#462b3f" nil))

      (red-dark        (sendai--palettize "#792c32" "#870000"))
      (orange-dark     (sendai--palettize "#7d452b" "#875f00"))
      (yellow-dark     (sendai--palettize "#796b2f" "#878700"))
      (green-dark      (sendai--palettize "#2a5d31" "#005f00"))
      (cyan-dark       (sendai--palettize "#185d5f" "#005f5f"))
      (blue-dark       (sendai--palettize "#245282" "#005f87"))
      (violet-dark     (sendai--palettize "#474881" "#5f5f87"))
      (magenta-dark    (sendai--palettize "#702e5e" "#5f005f"))

      (red-mid         (sendai--palettize "#ad3641" "#af0000"))
      (orange-mid      (sendai--palettize "#b0622e" "#af5f00"))
      (yellow-mid      (sendai--palettize "#b49635" "#afaf5f"))
      (green-mid       (sendai--palettize "#3d8638" "#5f875f"))
      (cyan-mid        (sendai--palettize "#1f8584" "#008787"))
      (blue-mid        (sendai--palettize "#2d77b6" "#0087af"))
      (violet-mid      (sendai--palettize "#6b63b5" "#5f5faf"))
      (magenta-mid     (sendai--palettize "#9c4080" "#875f87"))

      (red-primary     (sendai--palettize "#d33f4d" "#d70000" "brightred"))
      (orange-primary  (sendai--palettize "#d47732" "#d78700"))
      (yellow-primary  (sendai--palettize "#ddb63a" "#d7af5f" "brightyellow"))
      (green-primary   (sendai--palettize "#4ba33f" "#5faf5f" "brightgreen"))
      (cyan-primary    (sendai--palettize "#26a19f" "#00afaf" "cyan"))
      (blue-primary    (sendai--palettize "#3491dc" "#5f87d7" "blue"))
      (violet-primary  (sendai--palettize "#8578db" "#8787d7"))
      (magenta-primary (sendai--palettize "#bc4d99" "#af5f87" "magenta"))

      (red-light       (sendai--palettize "#e16a6d" "#d75f5f" "red"))
      (orange-light    (sendai--palettize "#ec9f63" "#ffaf5f"))
      (yellow-light    (sendai--palettize "#ded692" "#d7d787" "yellow"))
      (green-light     (sendai--palettize "#7cc36e" "#87af5f" "green"))
      (cyan-light      (sendai--palettize "#4edae5" "#5fd7d7" "brightcyan"))
      (blue-light      (sendai--palettize "#6cc2ff" "#5fafff" "brightblue"))
      (violet-light    (sendai--palettize "#afa2f2" "#afafff"))
      (magenta-light   (sendai--palettize "#db7fbb" "#d787af" "brightmagenta")))

  (custom-theme-set-faces
   'sendai

   ;; ----------
   ;; Core faces
   ;; ----------

   ;; Basics
   `(default ((,class (
      :background ,(when (or (display-graphic-p)
                             (not sendai-theme-inherit-tty-colors))
                     bg-primary)
      :foreground ,fg-primary))))
   `(cursor ((,class (:background ,fg-primary))))
   `(shadow ((,class (:foreground ,fg-darker))))
   `(link ((,class (:foreground ,blue-primary :underline t))))
   `(link-visited ((,class (:foreground ,violet-primary :underline t))))
   `(success ((,class (:foreground ,green-primary :weight bold))))
   `(warning ((,class (:foreground ,orange-primary :weight bold))))
   `(error ((,class (:foreground ,red-primary :weight bold))))
   `(escape-glyph ((,class (:foreground ,magenta-light :weight bold))))
   `(homoglyph ((,class (:foreground ,cyan-light))))
   `(nobreak-hyphen ((,class (:foreground ,magenta-light))))
   `(nobreak-space ((,class (:foreground ,magenta-light :underline t))))

   ;; Highlighting
   `(fringe ((,class (:background ,bg-primary))))
   `(highlight ((,class (:background ,cyan-primary :foreground ,fg-light))))
   `(region ((,class (:background ,blue-dark :foreground ,fg-primary))))
   `(secondary-selection ((,class (:background ,bg-lighter))))
   `(match ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(isearch ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(isearch-fail ((,class (:background ,red-dark))))
   `(lazy-highlight
     ((,class (:background ,fg-darker :foreground ,bg-primary))))
   `(completions-common-part ((,class (:foreground ,blue-primary))))
   `(show-paren-match
     ((,class (:background ,violet-dark :foreground ,fg-light :weight bold))))
   `(show-paren-match-expression ((,class (:background ,violet-darker))))
   `(show-paren-mismatch
     ((,class (:background ,red-primary :foreground ,fg-light))))
   `(hl-line ((,class (:background ,bg-light))))
   `(trailing-whitespace ((,class (:background ,red-primary))))

   ;; Mode line
   `(mode-line ((,class (:background ,bg-light :foreground ,fg-light))))
   `(mode-line-inactive ((,class (:background ,bg-dark :foreground ,fg-dark))))
   `(mode-line-highlight ((,class (:box ,fg-darker))))

   ;; Misc UI
   `(minibuffer-prompt ((,class (:foreground ,blue-primary :weight bold))))
   `(header-line ((,class (:background ,bg-lighter :foreground ,fg-primary))))
   `(header-line-highlight
     ((,class (:background ,fg-darker :foreground ,bg-primary))))
   `(line-number ((,class (:foreground ,fg-darker))))
   `(line-number-current-line ((,class (:foreground ,fg-primary))))
   `(line-number-major-tick
     ((,class (:background ,bg-lighter :foreground ,fg-dark))))
   `(line-number-minor-tick
     ((,class (:background ,bg-light :foreground ,fg-darker))))
   `(widget-field ((,class (:background ,bg-lighter))))
   `(widget-single-line-field ((,class (:background ,bg-lighter))))
   `(vertical-border ((,class (:foreground ,bg-lighter))))
   `(window-divider ((,class (:foreground ,bg-lighter))))
   `(window-divider-first-pixel ((,class (:foreground ,fg-darker))))
   `(window-divider-last-pixel ((,class (:foreground ,bg-dark))))
   `(tty-menu-enabled-face
     ((,class (:background ,bg-light :foreground ,fg-light))))
   `(tty-menu-disabled-face
     ((,class (:background ,bg-light :foreground ,fg-dark))))
   `(tty-menu-selected-face
     ((,class (:background ,cyan-primary :foreground ,fg-light))))

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

   ;; comint-mode
   `(comint-highlight-prompt ((,class (:foreground ,fg-darker :weight bold))))

   ;; compilation-mode
   `(compilation-error ((,class (:foreground ,red-primary :weight bold))))
   `(compilation-warning ((,class (:foreground ,yellow-primary :weight bold))))
   `(compilation-info ((,class (:foreground ,blue-primary :weight bold))))
   `(compilation-line-number ((,class (:foreground ,blue-light))))
   `(compilation-column-number ((,class (:foreground ,blue-light))))
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
   `(dired-header ((,class (:foreground ,blue-primary :weight bold))))
   `(dired-directory ((,class (:foreground ,blue-light :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan-light))))
   `(dired-special ((,class (:foreground ,yellow-light))))
   `(dired-marked ((,class (:foreground ,yellow-primary :weight bold))))
   `(dired-mark ((,class (:inherit dired-marked))))
   `(dired-flagged ((,class (:foreground ,red-primary :weight bold))))

   ;; eww
   `(eww-form-submit ((,class (
      :background ,fg-primary :foreground ,bg-primary
      :box (:line-width 1 :style released-button)))))
   `(eww-form-file ((,class (:inherit eww-form-submit))))
   `(eww-form-checkbox ((,class (
      :background ,fg-darker :foreground ,bg-primary
      :box (:line-width 1 :color ,fg-dark :style released-button)))))
   `(eww-form-select ((,class (:inherit eww-form-checkbox))))
   `(eww-form-text ((,class (
      :background ,bg-lighter :box (:line-width 1 :color ,fg-darker)))))
   `(eww-form-textarea ((,class (:background ,bg-lighter))))
   `(eww-valid-certificate ((,class :foreground ,green-light :weight bold)))
   `(eww-invalid-certificate ((,class :foreground ,red-primary :weight bold)))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,fg-darker :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,blue-light :weight bold))))
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

   ;; flymake
   `(flymake-error
     ((,class (:underline (:color ,red-primary :style wave)))))
   `(flymake-warning
     ((,class (:underline (:color ,yellow-primary :style wave)))))
   `(flymake-note
     ((,class (:underline (:color ,blue-primary :style wave)))))

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
   `(info-header-xref ((,class (:foreground ,blue-light :underline t))))

   ;; makefile-mode
   `(makefile-space ((,class (:background ,red-primary))))

   ;; message
   `(message-header-name ((,class (:foreground ,blue-light))))
   `(message-header-subject ((,class (:foreground ,fg-light :weight bold))))
   `(message-header-to ((,class (:foreground ,yellow-light :weight bold))))
   `(message-header-cc ((,class (:foreground ,green-light :weight bold))))
   `(message-header-newsgroups
     ((,class (:foreground ,yellow-primary :weight bold))))
   `(message-header-other ((,class (:foreground ,fg-primary))))
   `(message-header-xheader ((,class (:foreground ,blue-primary))))
   `(message-separator ((,class (:foreground ,fg-darker))))
   `(message-mml ((,class (:foreground ,yellow-primary))))
   `(message-cited-text-1 ((,class (:foreground ,violet-light))))
   `(message-cited-text-2 ((,class (:foreground ,green-light))))
   `(message-cited-text-3 ((,class (:foreground ,magenta-light))))
   `(message-cited-text-4 ((,class (:foreground ,cyan-light))))

   ;; org-mode
   ;; Note: `org-level-N' is inherited from `outline-N'.
   `(org-document-info-keyword ((,class (:inherit org-meta-line))))
   `(org-document-title
     ((,class (:foreground ,blue-light :weight bold))))
   `(org-document-info ((,class (:foreground ,blue-light))))
   `(org-ellipsis ((,class (:foreground ,fg-darker))))
   `(org-hide ((,class (:foreground ,bg-primary))))
   `(org-checkbox ((,class (:foreground ,fg-darker :weight bold))))
   `(org-latex-and-related ((,class (:foreground ,fg-darker))))
   `(org-footnote ((,class (:foreground ,blue-primary :underline t))))
   `(org-date ((,class (:foreground ,cyan-primary :underline t))))
   `(org-sexp-date ((,class (:foreground ,cyan-primary))))
   `(org-todo ((,class (:foreground ,red-light :weight bold))))
   `(org-done ((,class (:foreground ,green-light :weight bold))))
   `(org-headline-todo ((,class (:foreground ,red-primary :weight bold))))
   `(org-headline-done ((,class (:foreground ,green-primary))))
   `(org-tag ((,class (:foreground ,yellow-light :weight normal))))
   `(org-tag-group
     ((,class (:foreground ,yellow-light :weight normal :slant italic))))
   `(org-priority ((,class (:foreground ,orange-light))))
   `(org-formula ((,class (:foreground ,orange-light))))
   `(org-macro ((,class (:foreground ,violet-light))))
   `(org-block
     ((,class (:background ,blue-darker :foreground ,fg-primary :extend t))))
   `(org-quote ((,class (:slant italic))))
   `(org-verse ((,class (:slant italic))))
   `(org-table ((,class (:background ,blue-darker :foreground ,blue-light))))
   `(org-table-header
     ((,class (:background ,blue-dark :foreground ,fg-primary))))
   `(org-drawer ((,class (:foreground ,blue-light))))
   `(org-special-keyword ((,class (:foreground ,blue-primary))))
   `(org-date-selected
     ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(org-column ((,class (
      :background ,bg-light :box (:line-width -1 :style released-button)))))
   `(org-column-title ((,class (:background ,bg-lighter :weight bold))))
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
   `(org-mode-line-clock-overrun ((,class (:background ,red-primary))))

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

   ;; whitespace-mode
   `(whitespace-space ((,class (:foreground ,violet-primary))))
   `(whitespace-hspace ((,class (:inherit whitespace-space))))
   `(whitespace-tab ((,class (:inherit whitespace-space))))
   `(whitespace-newline ((,class (:foreground ,bg-lighter))))
   `(whitespace-line ((,class (:background ,magenta-dark))))
   `(whitespace-indentation
     ((,class (:background ,yellow-light :foreground ,bg-primary))))
   `(whitespace-empty ((,class (:inherit whitespace-indentation))))
   `(whitespace-space-after-tab ((,class (:inherit whitespace-indentation))))
   `(whitespace-big-indent
     ((,class (:background ,orange-primary :foreground ,fg-light))))
   `(whitespace-trailing
     ((,class (:background ,red-primary :foreground ,fg-light :weight bold))))
   `(whitespace-space-before-tab ((,class (:inherit whitespace-trailing))))

   ;; --------------------
   ;; Third-party packages
   ;; --------------------

   ;; company-mode
   `(company-preview
     ((,class (:foreground ,fg-primary :background ,blue-dark))))
   `(company-template-field
     ((,class (:foreground ,fg-primary :background ,bg-lighter))))
   `(company-tooltip
     ((,class (:foreground ,fg-primary :background ,bg-lighter))))
   `(company-tooltip-selection ((,class (:background ,blue-mid))))
   `(company-tooltip-search ((,class (:inherit isearch))))
   `(company-tooltip-search-selection ((,class (:inherit isearch))))
   `(company-scrollbar-fg ((,class (:background ,fg-darker))))
   `(company-scrollbar-bg ((,class (:background ,bg-light))))
   `(company-echo-common ((,class (:foreground ,yellow-primary))))
   `(company-tooltip-common ((,class (:foreground ,yellow-primary))))
   `(company-preview-common ((,class (:foreground ,yellow-primary))))
   `(company-tooltip-annotation ((,class (:foreground ,cyan-light))))

   ;; elfeed
   `(elfeed-search-title-face ((,class (:foreground ,fg-primary))))
   `(elfeed-search-unread-title-face ((,class (:weight bold :slant italic))))
   `(elfeed-search-date-face ((,class (:foreground ,fg-darker))))
   `(elfeed-search-feed-face ((,class (:foreground ,blue-primary))))
   `(elfeed-search-tag-face ((,class (:foreground ,yellow-light))))
   `(elfeed-search-unread-count-face ((,class (:foreground ,blue-light))))
   `(elfeed-log-date-face ((,class (:foreground ,cyan-primary))))
   `(elfeed-log-error-level-face ((,class (:foreground ,red-primary))))
   `(elfeed-log-warn-level-face ((,class (:foreground ,yellow-primary))))
   `(elfeed-log-info-level-face ((,class (:foreground ,blue-primary))))
   `(elfeed-log-debug-level-face ((,class (:foreground ,magenta-light))))

   ;; form-feed
   `(form-feed-line ((,(cons '(type graphic) class)
                      (:strike-through ,fg-darker))))

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

   ;; telephone-line
   `(telephone-line-accent-active ((,class (:background ,blue-mid))))
   `(telephone-line-accent-inactive ((,class (:background ,blue-dark))))

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

;;; sendai-theme.el ends here
