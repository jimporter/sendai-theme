;;; sendai-theme.el --- A cool blue color theme -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/sendai-theme
;; Version: 0.1-git
;; Keywords:
;; Package-Requires: ((emacs "27.1"))

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

(deftheme sendai "A cool blue color theme.")

(defvar sendai--face-classes
  '((true-color ((class color) (min-colors 16777216))       . 0)
    (256-color  ((class color) (min-colors 256))            . 1)
    (tty-color  ((class color) (min-colors 256) (type tty)) . 2))
  "An alist of possible face classes.
Keys are descriptive names and map to conses of face classes and their
corresponding index into `sendai--color' lists.")

(defgroup sendai-theme nil
  "Sendai-theme options."
  :group 'faces)

(defcustom sendai-theme-inherit-tty-colors nil
  "Use basic TTY colors when in xterm-256color mode."
  :type 'boolean
  :group 'sendai-theme)

(defcustom sendai-default-class nil
  "The default face class to use when setting non-`defface' colors.
Some theme settings only support a single color value.  When using
`emacsclient' in multiple settings (e.g. GUI and TTY frames), this can
cause colors to look inconsistent.  With the default value of nil,
this uses the setting matching the theme was initially loaded.  With
any other value, force the theme to use the colors associated with
that face class.

For example, when using `emacs --daemon' in a GUI environment, you
likely want to set this to `true-color'."
  :type '(choice (const :tag "Default"    nil)
                 (const :tag "True color" true-color)
                 (const :tag "256 color"  256-color)
                 (const :tag "TTY color"  tty-color))
  :group 'sendai-theme)

(defcustom sendai-tab-padding
  (if (>= emacs-major-version 28) '(4 . 2) 2)
  "The default padding around tabs in `tab-bar-mode' and `tab-line-mode'.
This is passed to the `:box' face attribute and can be either an
integer (to use the same width in all directions) or a pair of
integers (to use separate widths on the X and Y axes)."
  :type '(choice integer (cons integer integer))
  :group 'sendai-theme)

;; Define some faces.  Note that the actual styling is done below.  Doing things
;; this way makes it easier to define each face with multiple face specs for
;; better `emacsclient' support.

(defface sendai-hl-todo-error nil
  "A face used to highlight error-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(defface sendai-hl-todo-warning nil
  "A face used to highlight warning-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(defface sendai-hl-todo-success nil
  "A face used to highlight success-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(eval-and-compile
  (defun sendai--color (color-true color-256 &optional color-tty)
    "Generate a color spec holding values for various environments.
COLOR-TRUE is the color to use in true color (24-bit) modes, COLOR-256
the color to use in 8-bit modes, and COLOR-TTY the color to use in
8-bit TTY modes.  If COLOR-TTY isn't specified, COLOR-256 is used for
TTY modes instead."
    `'(sendai--color ,color-true ,color-256 ,(or color-tty color-256)))

  (defconst sendai-palette
    `((bg-darker       ,(sendai--color "#161c23" "#1c1c1c"))
      (bg-dark         ,(sendai--color "#1c242e" "#262626"))
      (bg-primary      ,(sendai--color "#232c38" "#303030"))
      (bg-light        ,(sendai--color "#364454" "#444444"))
      (bg-lighter      ,(sendai--color "#4a5b71" "#5f5f5f" "black"))

      (fg-darker       ,(sendai--color "#7b96b7" "#5f87af" "brightblack"))
      (fg-dark         ,(sendai--color "#a2b6d0" "#87afd7"))
      (fg-primary      ,(sendai--color "#c9d6e9" "#d7d7d7" "white"))
      (fg-light        ,(sendai--color "#f0f6fe" "#ffffff" "brightwhite"))

      (red-darker      ,(sendai--color "#4f282a" 'unspecified))
      (orange-darker   ,(sendai--color "#4d3225" 'unspecified))
      (yellow-darker   ,(sendai--color "#4e462b" 'unspecified))
      (green-darker    ,(sendai--color "#2b402e" 'unspecified))
      (cyan-darker     ,(sendai--color "#204142" 'unspecified))
      (blue-darker     ,(sendai--color "#273b55" 'unspecified))
      (violet-darker   ,(sendai--color "#36354f" 'unspecified))
      (magenta-darker  ,(sendai--color "#462b3f" 'unspecified))

      (red-dark        ,(sendai--color "#792c32" "#870000"))
      (orange-dark     ,(sendai--color "#7d452b" "#875f00"))
      (yellow-dark     ,(sendai--color "#796b2f" "#878700"))
      (green-dark      ,(sendai--color "#2a5d31" "#005f00"))
      (cyan-dark       ,(sendai--color "#185d5f" "#005f5f"))
      (blue-dark       ,(sendai--color "#245282" "#005f87"))
      (violet-dark     ,(sendai--color "#484982" "#5f5f87"))
      (magenta-dark    ,(sendai--color "#702e5e" "#5f005f"))

      (red-mid         ,(sendai--color "#ad3641" "#af0000"))
      (orange-mid      ,(sendai--color "#b0622e" "#af5f00"))
      (yellow-mid      ,(sendai--color "#b49635" "#afaf5f"))
      (green-mid       ,(sendai--color "#3d8638" "#5f875f"))
      (cyan-mid        ,(sendai--color "#1f8584" "#008787"))
      (blue-mid        ,(sendai--color "#2d77b6" "#0087af"))
      (violet-mid      ,(sendai--color "#6b63b5" "#5f5faf"))
      (magenta-mid     ,(sendai--color "#9c4080" "#875f87"))

      (red-primary     ,(sendai--color "#d33f4d" "#d70000" "brightred"))
      (orange-primary  ,(sendai--color "#d47732" "#d78700"))
      (yellow-primary  ,(sendai--color "#ddb63a" "#d7af5f" "brightyellow"))
      (green-primary   ,(sendai--color "#4ba33f" "#5faf5f" "brightgreen"))
      (cyan-primary    ,(sendai--color "#26a19f" "#00afaf" "cyan"))
      (blue-primary    ,(sendai--color "#3491dc" "#5f87d7" "blue"))
      (violet-primary  ,(sendai--color "#8578db" "#8787d7"))
      (magenta-primary ,(sendai--color "#bc4d99" "#af5f87" "magenta"))

      (red-light       ,(sendai--color "#e16a6d" "#d75f5f" "red"))
      (orange-light    ,(sendai--color "#ec9f63" "#ffaf5f"))
      (yellow-light    ,(sendai--color "#ded692" "#d7d787" "yellow"))
      (green-light     ,(sendai--color "#7cc36e" "#87af5f" "green"))
      (cyan-light      ,(sendai--color "#4edae5" "#5fd7d7" "brightcyan"))
      (blue-light      ,(sendai--color "#6cc2ff" "#5fafff" "brightblue"))
      (violet-light    ,(sendai--color "#afa2f2" "#afafff"))
      (magenta-light   ,(sendai--color "#db7fbb" "#d787af" "brightmagenta")))
    "An alist of colors defining the Sendai theme's palette.
Each value is a `sendai--color'."))


;; Utility functions

(defun sendai--default-tty-color (color)
  "Modify COLOR to indicate that the TTY-mode color should be the default.
This is useful for inheriting the default foreground/background from
the terminal."
  (append (butlast color) '(unspecified)))

(defun sendai-active-class ()
  "Get the active face class to use when setting non-`defface' colors."
  (or sendai-default-class
      (let ((colors (display-color-cells)))
        (cond
         ((>= colors 16777216) 'true-color)
         ((>= colors 256) (if (display-graphic-p) '256-color 'tty-color))))))

(defun sendai--subst-p (tree)
  "Return non-nil if TREE has a substitutable color value.
See `sendai--color'."
  (cond ((consp tree)
         (cond ((eq (car tree) 'sendai--color)
                t)
               ((proper-list-p tree)
                (seq-some #'sendai--subst-p tree))
               (t
                (or (sendai--subst-p (car tree))
                    (sendai--subst-p (cdr tree))))))
        ((vectorp tree)
         (seq-some #'sendai--subst-p tree))
        (t nil)))

(defun sendai--do-subst (tree index)
  "Replace abstract colors with their real values.
TREE is an object containing colors generated by `sendai--color'.
For each occurence of such a color, replace it with the real
value specified by INDEX."
  (cond ((consp tree)
         (cond ((eq (car tree) 'sendai--color)
                (nth (1+ index) tree))
               ((proper-list-p tree)
                (mapcar (lambda (i) (sendai--do-subst i index)) tree))
               (t
                (let ((a (sendai--do-subst (car tree) index))
                      (d (sendai--do-subst (cdr tree) index)))
                  (if (and (eq a (car tree)) (eq d (cdr tree)))
                      tree (cons a d))))))
        ((vectorp tree)
         (apply #'vector (mapcar (lambda (i) (sendai--do-subst i index)) tree)))
        (t tree)))

(defun sendai-subst (attrs &optional class-name)
  "Replace abstract color definitions with their real values.
ATTRS is an object containing colors generated by `sendai--color'.
For each occurence of such a color, replace it with the real
value specified by CLASS-NAME (a key in `sendai--face-classes');
if CLASS-NAME is nil, use `(sendai-active-class)' instead."
  (let* ((class-name (or class-name (sendai-active-class)))
         (index (cdr (alist-get class-name sendai--face-classes))))
    (sendai--do-subst attrs index)))

(defun sendai--parameterize (class-names display-extra attrs)
  "Parameterize abstract color definitions according to CLASS-NAMES.
ATTRS is a face attribute list, and DISPLAY-EXTRA is an alist of
extra classifications to apply to the resulting list.  If any
classification in DISPLAY-EXTRA conflicts with one in
CLASS-NAMES, that display class is ignored entirely."
  (when attrs
    (cond
     ((not (sendai--subst-p attrs))
      `((,display-extra . ,attrs)))
     ((memq display-extra '(default t))
      (error "Abstract color definitions not allowed here"))
     (t
      (or (mapcan
           (lambda (class-name)
             (let* ((entry (alist-get class-name sendai--face-classes))
                    (display-class (car entry)))
               (catch 'conflict
                 ;; Make sure the extra display rules don't conflict with the
                 ;; main display class.
                 (dolist (i display-extra)
                   (when (assq (car i) display-class)
                     (throw 'conflict nil)))
                 `((,(append display-extra display-class)
                    . ,(sendai--do-subst attrs (cdr entry)))))))
           class-names)
          (error "Unable to parameterize color definitions"))))))

(defun sendai--filter-face-attrs (attrs)
  "Filter a face attribute list (ATTRS) into constant and substitutable parts."
  (let (const-attrs subst-attrs)
    (while attrs
      (let ((key (pop attrs))
            (value (pop attrs)))
        (if (sendai--subst-p value)
            (setq subst-attrs (append subst-attrs (list key value)))
          (setq const-attrs (append const-attrs (list key value))))))
    (cons const-attrs subst-attrs)))

(defun sendai-make-face (&rest spec)
  "Fill a face spec (SPEC) with real colors."
  (let ((class-names (if sendai-theme-inherit-tty-colors
                         '(true-color tty-color 256-color)
                       '(true-color 256-color))))
    (mapcan
     (lambda (elem)
       (sendai--parameterize class-names (car elem) (cdr elem)))
     spec)))

(defun sendai-face (&rest attrs)
  "Fill a face attribute list (ATTRS) with real color, the easy way.
If ATTRS contains any nonparameterized attributes, this function
will split them out and apply them using the `default' display
rule."
  (let ((filtered (sendai--filter-face-attrs attrs)))
    (sendai-make-face `(default . ,(car filtered))
                      `(nil . ,(cdr filtered)))))

(defmacro sendai-let-palette (&rest body)
  "Evaluate BODY with all the entries of `sendai-palette' in scope."
  (declare (indent 0))
  `(let ,sendai-palette
     (ignore ,@(mapcar #'car sendai-palette))
     ,@body))


;; Face customizations

(sendai-let-palette
  (custom-theme-set-faces
   'sendai

   ;; ----------
   ;; Core faces
   ;; ----------

   ;; Basics
   `(default ,(sendai-face :background (sendai--default-tty-color bg-primary)
                           :foreground fg-primary))
   `(cursor ,(sendai-face :background fg-primary))
   `(shadow ,(sendai-face :foreground fg-darker))
   `(link ,(sendai-face :foreground blue-primary :underline t))
   `(link-visited ,(sendai-face :foreground violet-primary :underline t))
   `(success ,(sendai-face :foreground green-primary :weight 'bold))
   `(warning ,(sendai-face :foreground orange-primary :weight 'bold))
   `(error ,(sendai-face :foreground red-primary :weight 'bold))
   `(escape-glyph
     ,(sendai-face :foreground magenta-light :weight 'bold
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (1 . -1) :color ,magenta-dark))))
   `(homoglyph ,(sendai-face :foreground cyan-light))
   `(nobreak-space ,(sendai-face :underline `(:color ,magenta-primary)))
   `(nobreak-hyphen ,(sendai-face :inherit 'nobreak-space))
   `(separator-line ,(sendai-face :foreground fg-darker))
   `(help-argument-name ,(sendai-face :foreground blue-light))
   `(help-key-binding
     ,(sendai-face :background blue-darker :foreground blue-light
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (1 . -1) :color ,blue-dark))))

   ;; Highlighting
   `(fringe ,(sendai-face :background bg-primary))
   `(highlight ,(sendai-face :background blue-mid :foreground fg-light))
   `(region ,(sendai-face :background blue-dark :foreground fg-primary))
   `(secondary-selection ,(sendai-face :background bg-lighter))
   `(match ,(sendai-face :background yellow-light :foreground bg-primary
                         :distant-foreground fg-primary))
   `(isearch ,(sendai-face :background yellow-light :foreground bg-primary))
   `(isearch-fail ,(sendai-face :background red-dark))
   `(isearch-group-1 ,(sendai-face :background green-light
                                   :foreground bg-primary))
   `(isearch-group-2 ,(sendai-face :background green-primary
                                   :foreground bg-primary))
   `(lazy-highlight ,(sendai-face :background fg-darker :foreground bg-primary))
   `(completions-common-part ,(sendai-face :foreground yellow-primary))
   `(completions-highlight ,(sendai-face :background blue-mid))
   `(show-paren-match
     ,(sendai-face :background violet-dark :foreground fg-light :weight 'bold
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (-1 . -1) :color ,violet-mid))))
   `(show-paren-match-expression ,(sendai-face :background violet-darker))
   `(show-paren-mismatch
     ,(sendai-face :background red-mid :foreground fg-light :weight 'bold
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (-1 . -1) :color ,red-primary))))
   `(hl-line ,(sendai-face :background bg-light))
   `(bookmark-face ,(sendai-face :foreground magenta-light))
   `(trailing-whitespace ,(sendai-face :background red-primary))

   ;; Mode line
   `(mode-line ,(sendai-face :background bg-light :foreground fg-light))
   `(mode-line-inactive ,(sendai-face :background bg-darker
                                      :foreground fg-dark))
   `(mode-line-highlight ,(sendai-face :box fg-darker))

   ;; Misc UI
   `(minibuffer-prompt ,(sendai-face :foreground blue-primary :weight 'bold))
   `(header-line ,(sendai-face :background bg-lighter :foreground fg-primary))
   `(header-line-highlight ,(sendai-face :background fg-darker
                                         :foreground bg-primary))
   `(line-number ,(sendai-face :foreground fg-darker))
   `(line-number-current-line ,(sendai-face :foreground fg-primary))
   `(line-number-major-tick ,(sendai-face :background bg-lighter
                                          :foreground fg-dark))
   `(line-number-minor-tick ,(sendai-face :background bg-light
                                          :foreground fg-darker))
   `(widget-field ,(sendai-face :background bg-lighter))
   `(widget-single-line-field ,(sendai-face :background bg-lighter))
   `(vertical-border ,(sendai-face :foreground bg-lighter))
   `(window-divider ,(sendai-face :foreground bg-lighter))
   `(window-divider-first-pixel ,(sendai-face :foreground fg-darker))
   `(window-divider-last-pixel ,(sendai-face :foreground bg-darker))
   `(tty-menu-enabled-face ,(sendai-face :background bg-light
                                         :foreground fg-light))
   `(tty-menu-disabled-face ,(sendai-face :background bg-light
                                          :foreground fg-dark))
   `(tty-menu-selected-face ,(sendai-face :background blue-mid
                                          :foreground fg-light))

   ;; Font lock
   `(font-lock-builtin-face ,(sendai-face :foreground blue-light))
   `(font-lock-comment-face ,(sendai-face :foreground fg-darker))
   `(font-lock-doc-face ,(sendai-face :inherit 'font-lock-comment-face))
   `(font-lock-doc-markup-face ,(sendai-face :foreground cyan-primary))
   `(font-lock-constant-face ,(sendai-face :foreground cyan-primary))
   `(font-lock-function-name-face ,(sendai-face :foreground yellow-primary
                                                :weight 'bold))
   `(font-lock-keyword-face ,(sendai-face :foreground blue-primary))
   `(font-lock-negation-char-face ,(sendai-face :foreground yellow-light))
   `(font-lock-regexp-grouping-construct
     ,(sendai-face :foreground yellow-light))
   `(font-lock-regexp-grouping-backslash
     ,(sendai-face :foreground cyan-primary))
   `(font-lock-string-face ,(sendai-face :foreground green-light))
   `(font-lock-type-face ,(sendai-face :foreground cyan-light))
   `(font-lock-variable-name-face ,(sendai-face :foreground yellow-light))
   `(font-lock-warning-face ,(sendai-face :foreground red-primary))

   ;; -----------------
   ;; Built-in packages
   ;; -----------------

   ;; ansi-color
   `(ansi-color-black ,(sendai-face :background bg-lighter
                                    :foreground bg-lighter))
   `(ansi-color-red ,(sendai-face :background red-light
                                  :foreground red-light))
   `(ansi-color-green ,(sendai-face :background green-light
                                    :foreground green-light))
   `(ansi-color-yellow ,(sendai-face :background yellow-light
                                     :foreground yellow-light))
   `(ansi-color-blue ,(sendai-face :background blue-primary
                                   :foreground blue-primary))
   `(ansi-color-magenta ,(sendai-face :background magenta-primary
                                      :foreground magenta-primary))
   `(ansi-color-cyan ,(sendai-face :background cyan-primary
                                   :foreground cyan-primary))
   `(ansi-color-white ,(sendai-face :background fg-primary
                                    :foreground fg-primary))
   `(ansi-color-bright-black ,(sendai-face :background fg-darker
                                           :foreground fg-darker))
   `(ansi-color-bright-red ,(sendai-face :background red-primary
                                         :foreground red-primary))
   `(ansi-color-bright-green ,(sendai-face :background green-primary
                                           :foreground green-primary))
   `(ansi-color-bright-yellow ,(sendai-face :background yellow-primary
                                            :foreground yellow-primary))
   `(ansi-color-bright-blue ,(sendai-face :background blue-light
                                          :foreground blue-light))
   `(ansi-color-bright-magenta ,(sendai-face :background magenta-light
                                             :foreground magenta-light))
   `(ansi-color-bright-cyan ,(sendai-face :background cyan-light
                                          :foreground cyan-light))
   `(ansi-color-bright-white ,(sendai-face :background fg-light
                                           :foreground fg-light))

   ;; calendar
   `(calendar-month-header ,(sendai-face :foreground yellow-primary
                                         :weight 'bold))
   `(calendar-weekday-header ,(sendai-face :foreground blue-primary))
   `(calendar-weekend-header ,(sendai-face :foreground blue-light))
   `(diary ,(sendai-face :foreground cyan-light))

   ;; change-log-mode
   `(change-log-date ,(sendai-face :foreground cyan-primary))
   `(change-log-name ,(sendai-face :foreground yellow-light))
   `(change-log-email ,(sendai-face :foreground blue-primary))
   `(change-log-file ,(sendai-face :foreground yellow-primary :weight 'bold))
   `(change-log-list ,(sendai-face :foreground blue-light))
   `(change-log-conditionals ,(sendai-face :foreground green-light))
   `(change-log-function ,(sendai-face :foreground green-light))
   `(change-log-acknowledgment ,(sendai-face :foreground fg-darker))

   ;; comint-mode
   `(comint-highlight-prompt ,(sendai-face :foreground fg-darker :weight 'bold))

   ;; compilation-mode
   `(compilation-error ,(sendai-face :foreground red-primary :weight 'bold))
   `(compilation-warning ,(sendai-face :foreground yellow-primary
                                       :weight 'bold))
   `(compilation-info ,(sendai-face :foreground blue-primary :weight 'bold))
   `(compilation-line-number ,(sendai-face :foreground blue-light))
   `(compilation-column-number ,(sendai-face :foreground blue-light))
   `(compilation-mode-line-exit ,(sendai-face :foreground green-primary
                                              :weight 'bold))
   `(compilation-mode-line-fail ,(sendai-face :foreground red-primary
                                              :weight 'bold))
   `(compilation-mode-line-run ,(sendai-face :foreground yellow-primary
                                             :weight 'bold))

   ;; cua-mode
   `(cua-global-mark ,(sendai-face :background yellow-primary
                                   :foreground bg-primary))
   `(cua-rectangle ,(sendai-face :inherit 'region))
   `(cua-rectangle-noselect ,(sendai-face :inherit 'secondary-selection))

   ;; customize
   `(custom-group-tag ,(sendai-face :foreground yellow-primary :weight 'bold
                                    :height 1.2))
   `(custom-group-tag-1 ,(sendai-face :foreground cyan-light :weight 'bold
                                      :height 1.2))
   `(custom-state ,(sendai-face :foreground green-primary))
   `(custom-changed ,(sendai-face :foreground yellow-light))
   `(custom-modified ,(sendai-face :foreground yellow-light))
   `(custom-themed ,(sendai-face :foreground yellow-light))
   `(custom-set ,(sendai-face :background green-light :foreground bg-primary))
   `(custom-invalid ,(sendai-face :foreground red-primary :weight 'bold
                                  :underline '(:style wave)))
   `(custom-rogue ,(sendai-face :foreground orange-primary :weight 'bold
                                :underline '(:style wave)))
   `(custom-variable-tag ,(sendai-face :foreground blue-light :weight 'bold))
   `(custom-variable-obsolete ,(sendai-face :foreground fg-darker
                                            :weight 'bold))
   `(custom-button
     ,(sendai-face :background fg-primary :foreground bg-primary
                   :box '(:line-width 1 :style released-button)))
   `(custom-button-mouse
     ,(sendai-face :background fg-light :foreground bg-primary
                   :box '(:line-width 1 :style released-button)))
   `(custom-button-pressed
     ,(sendai-face :background fg-primary :foreground bg-primary
                   :box '(:line-width 1 :style pressed-button)))
   `(custom-button-unraised ,(sendai-face :inherit 'default :underline t))
   `(custom-button-pressed-unraised
     ,(sendai-face :inherit 'custom-button-unraised :foreground violet-primary))

   ;; diff-mode
   `(diff-header ,(sendai-face :foreground fg-primary))
   `(diff-file-header ,(sendai-face :foreground yellow-primary :weight 'bold))
   `(diff-hunk-header ,(sendai-face :foreground cyan-light))
   `(diff-error ,(sendai-face :foreground red-primary :weight 'bold
                              :underline '(:style wave)))
   `(diff-added ,(sendai-face :background green-darker))
   `(diff-removed ,(sendai-face :background red-darker))
   `(diff-changed ,(sendai-face :background yellow-darker))
   `(diff-changed-unspecified ,(sendai-face :background yellow-darker))
   `(diff-indicator-added ,(sendai-face :foreground green-light
                                        :inherit 'diff-added))
   `(diff-indicator-removed ,(sendai-face :foreground red-light
                                          :inherit 'diff-removed))
   `(diff-indicator-changed ,(sendai-face :foreground yellow-light
                                          :inherit 'diff-changed))
   `(diff-refine-added ,(sendai-face :background green-dark))
   `(diff-refine-removed ,(sendai-face :background red-dark))
   `(diff-refine-changed ,(sendai-face :background yellow-dark))

   ;; dired
   `(dired-header ,(sendai-face :foreground blue-light :weight 'bold))
   `(dired-directory ,(sendai-face :foreground blue-primary :weight 'bold))
   `(dired-symlink ,(sendai-face :foreground cyan-light))
   `(dired-broken-symlink ,(sendai-face
                            :underline `(:color ,red-primary :style wave)
                            :inherit 'dired-symlink))
   `(dired-special ,(sendai-face :foreground yellow-light))
   `(dired-marked ,(sendai-face :foreground yellow-primary :weight 'bold))
   `(dired-mark ,(sendai-face :inherit 'dired-marked))
   `(dired-flagged ,(sendai-face :foreground red-primary :weight 'bold))

   ;; ediff
   `(ediff-even-diff-Ancestor ,(sendai-face :background blue-darker))
   `(ediff-even-diff-A ,(sendai-face :background blue-darker))
   `(ediff-even-diff-B ,(sendai-face :background blue-darker))
   `(ediff-even-diff-C ,(sendai-face :background blue-darker))
   `(ediff-odd-diff-Ancestor ,(sendai-face :background blue-darker))
   `(ediff-odd-diff-A ,(sendai-face :background blue-darker))
   `(ediff-odd-diff-B ,(sendai-face :background blue-darker))
   `(ediff-odd-diff-C ,(sendai-face :background blue-darker))
   `(ediff-current-diff-Ancestor ,(sendai-face :background yellow-darker))
   `(ediff-current-diff-A ,(sendai-face :background red-darker))
   `(ediff-current-diff-B ,(sendai-face :background green-darker))
   `(ediff-current-diff-C ,(sendai-face :background blue-darker))
   `(ediff-fine-diff-Ancestor ,(sendai-face :background yellow-dark))
   `(ediff-fine-diff-A ,(sendai-face :background red-dark))
   `(ediff-fine-diff-B ,(sendai-face :background green-dark))
   `(ediff-fine-diff-C ,(sendai-face :background blue-dark))

   ;; emacs-authors-mode
   `(emacs-authors-default ,(sendai-face :foreground fg-darker :slant 'italic))
   `(emacs-authors-author ,(sendai-face :foreground yellow-primary
                                        :weight 'bold))
   `(emacs-authors-descriptor ,(sendai-face :foreground blue-light))

   ;; emacs-news-mode
   `(emacs-news-is-documented
     ,(sendai-face :foreground green-light :weight 'bold))
   `(emacs-news-does-not-need-documentation
     ,(sendai-face :foreground fg-darker :weight 'bold))

   ;; ert
   `(ert-test-result-expected ,(sendai-face :background green-primary
                                            :foreground fg-light))
   `(ert-test-result-unexpected ,(sendai-face :background red-primary
                                              :foreground fg-light))

   ;; eww
   `(eww-form-submit
     ,(sendai-face :background fg-primary :foreground bg-primary
                   :box '(:line-width 1 :style released-button)))
   `(eww-form-file ,(sendai-face :inherit 'eww-form-submit))
   `(eww-form-checkbox
     ,(sendai-face :background fg-darker :foreground bg-primary
                   :box `(:line-width 1 :color ,fg-dark
                          :style released-button)))
   `(eww-form-select ,(sendai-face :inherit 'eww-form-checkbox))
   `(eww-form-text ,(sendai-face :background bg-lighter
                                 :box `(:line-width 1 :color ,fg-darker)))
   `(eww-form-textarea ,(sendai-face :background bg-lighter))
   `(eww-valid-certificate ,(sendai-face :foreground green-light :weight 'bold))
   `(eww-invalid-certificate ,(sendai-face :foreground red-primary
                                           :weight 'bold))

   ;; eshell
   `(eshell-prompt ,(sendai-face :foreground fg-darker :weight 'bold))
   `(eshell-ls-directory ,(sendai-face :foreground blue-primary :weight 'bold))
   `(eshell-ls-archive ,(sendai-face :foreground magenta-primary :weight 'bold))
   `(eshell-ls-executable ,(sendai-face :foreground green-light))
   `(eshell-ls-product ,(sendai-face :foreground cyan-primary))
   `(eshell-ls-readonly ,(sendai-face :foreground orange-light))
   `(eshell-ls-symlink ,(sendai-face :foreground cyan-light))
   `(eshell-ls-special ,(sendai-face :foreground yellow-light))
   `(eshell-ls-backup ,(sendai-face :foreground fg-darker))
   `(eshell-ls-missing ,(sendai-face :foreground red-primary :weight 'bold))
   `(eshell-ls-clutter ,(sendai-face :foreground red-primary :weight 'bold))
   `(eshell-ls-unreadable ,(sendai-face :foreground orange-primary))

   ;; flymake
   `(flymake-error ,(sendai-face :underline `(:color ,red-primary :style wave)))
   `(flymake-warning
     ,(sendai-face :underline `(:color ,yellow-primary :style wave)))
   `(flymake-note ,(sendai-face :underline `(:color ,blue-primary :style wave)))

   ;; flyspell
   `(flyspell-duplicate
     ,(sendai-face :underline `(:color ,orange-primary :style wave)))
   `(flyspell-incorrect
     ,(sendai-face :underline `(:color ,red-primary :style wave)))

   ;; gdb
   `(breakpoint-enabled ,(sendai-face :foreground red-primary))
   `(breakpoint-disabled ,(sendai-face :foreground fg-darker))

   ;; hexl-mode
   `(hexl-address-region ,(sendai-face :background blue-darker
                                       :foreground fg-primary))
   `(hexl-ascii-region ,(sendai-face :inherit 'hexl-address-region))

   ;; hi-lock-mode
   `(hi-yellow ,(sendai-face :background yellow-primary :foreground bg-primary))
   `(hi-pink ,(sendai-face :background red-light :foreground bg-primary))
   `(hi-green ,(sendai-face :background green-light :foreground bg-primary))
   `(hi-blue ,(sendai-face :background blue-light :foreground bg-primary))
   `(hi-salmon ,(sendai-face :background orange-primary :foreground bg-primary))
   `(hi-aquamarine ,(sendai-face :background cyan-light :foreground bg-primary))
   `(hi-black-b ,(sendai-face :weight 'bold))
   `(hi-blue-b ,(sendai-face :foreground blue-primary :weight 'bold))
   `(hi-green-b ,(sendai-face :foreground green-primary :weight 'bold))
   `(hi-red-b ,(sendai-face :foreground red-primary :weight 'bold))
   `(hi-black-hb ,(sendai-face :foreground fg-light :weight 'bold))

   ;; image-dired
   `(image-dired-thumb-header-file-size ,(sendai-face :foreground blue-light))
   `(image-dired-thumb-mark ,(sendai-face :background yellow-primary))
   `(image-dired-thumb-flagged ,(sendai-face :background red-primary))

   ;; info-mode
   `(info-title-1 ,(sendai-face :height 1.5 :weight 'bold))
   `(info-title-2 ,(sendai-face :height 1.4 :weight 'bold))
   `(info-title-3 ,(sendai-face :height 1.3 :weight 'bold))
   `(info-title-4 ,(sendai-face :height 1.1 :weight 'bold))
   `(info-menu-header ,(sendai-face :weight 'bold))
   `(info-menu-star ,(sendai-face :foreground yellow-primary))
   `(info-node ,(sendai-face :foreground fg-light :weight 'bold :slant 'italic))
   `(info-header-xref ,(sendai-face :foreground blue-light :underline t))

   ;; makefile-mode
   `(makefile-space ,(sendai-face :background red-primary))

   ;; message
   `(message-header-name ,(sendai-face :foreground blue-light))
   `(message-header-subject ,(sendai-face :foreground fg-light :weight 'bold))
   `(message-header-to ,(sendai-face :foreground yellow-light :weight 'bold))
   `(message-header-cc ,(sendai-face :foreground green-light :weight 'bold))
   `(message-header-newsgroups ,(sendai-face :foreground yellow-primary
                                             :weight 'bold))
   `(message-header-other ,(sendai-face :foreground fg-primary))
   `(message-header-xheader ,(sendai-face :foreground blue-primary))
   `(message-separator ,(sendai-face :foreground fg-darker))
   `(message-mml ,(sendai-face :foreground yellow-primary))
   `(message-cited-text-1 ,(sendai-face :foreground violet-light))
   `(message-cited-text-2 ,(sendai-face :foreground green-light))
   `(message-cited-text-3 ,(sendai-face :foreground magenta-light))
   `(message-cited-text-4 ,(sendai-face :foreground cyan-light))

   ;; org-mode
   ;; Note: `org-level-N' is inherited from `outline-N'.
   `(org-document-info-keyword ,(sendai-face :inherit 'org-meta-line))
   `(org-document-title ,(sendai-face :foreground blue-light :weight 'bold))
   `(org-document-info ,(sendai-face :foreground blue-light))
   `(org-ellipsis ,(sendai-face :foreground fg-darker))
   `(org-hide ,(sendai-face :foreground bg-primary))
   `(org-checkbox ,(sendai-face :foreground fg-darker :weight 'bold))
   `(org-code ,(sendai-face :foreground cyan-primary :inherit 'fixed-pitch))
   `(org-verbatim ,(sendai-face :foreground cyan-primary :inherit 'fixed-pitch))
   `(org-latex-and-related ,(sendai-face :foreground fg-darker))
   `(org-target
     ,(sendai-face :background bg-light :foreground fg-dark
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (-1 . -1) :color ,fg-darker))))
   `(org-footnote ,(sendai-face :foreground blue-primary :underline t))
   `(org-date ,(sendai-face :foreground violet-primary :underline t))
   `(org-sexp-date ,(sendai-face :foreground cyan-primary))
   `(org-todo ,(sendai-face :foreground red-light :weight 'bold))
   `(org-done ,(sendai-face :foreground green-light :weight 'bold))
   `(org-headline-todo ,(sendai-face :foreground red-primary :weight 'bold))
   `(org-headline-done ,(sendai-face :foreground green-primary))
   `(org-tag ,(sendai-face :foreground yellow-light :weight 'normal))
   `(org-tag-group ,(sendai-face :foreground yellow-light :weight 'normal
                                 :slant 'italic))
   `(org-priority ,(sendai-face :foreground orange-light))
   `(org-formula ,(sendai-face :foreground orange-light))
   `(org-macro ,(sendai-face :foreground violet-light))
   `(org-block ,(sendai-face :background blue-darker :foreground fg-primary
                             :extend t))
   `(org-quote ,(sendai-face :slant 'italic))
   `(org-verse ,(sendai-face :slant 'italic))
   `(org-table ,(sendai-face :background blue-darker :foreground blue-light))
   `(org-table-header ,(sendai-face :background blue-dark
                                    :foreground fg-primary))
   `(org-drawer ,(sendai-face :foreground blue-light))
   `(org-special-keyword ,(sendai-face :foreground blue-primary))
   `(org-date-selected ,(sendai-face :background yellow-light
                                     :foreground bg-primary))
   `(org-column ,(sendai-face :background bg-light
                              :box '(:line-width -1 :style released-button)))
   `(org-column-title ,(sendai-face :background bg-lighter :weight 'bold))
   `(org-agenda-structure ,(sendai-face :foreground fg-darker))
   `(org-agenda-date ,(sendai-face :foreground blue-primary))
   `(org-agenda-date-weekend ,(sendai-face :foreground blue-light))
   `(org-agenda-date-today ,(sendai-face :foreground yellow-light))
   `(org-time-grid ,(sendai-face :foreground fg-darker))
   `(org-agenda-current-time ,(sendai-face :foreground yellow-light))
   `(org-agenda-done ,(sendai-face :foreground violet-primary))
   `(org-scheduled ,(sendai-face :foreground violet-light))
   `(org-scheduled-previously ,(sendai-face :foreground orange-light))
   `(org-scheduled-today ,(sendai-face :foreground violet-light))
   `(org-mode-line-clock-overrun ,(sendai-face :background red-primary))
   `(org-habit-clear-face ,(sendai-face :background blue-darker
                                        :foreground fg-light))
   `(org-habit-clear-future-face ,(sendai-face :background blue-dark
                                               :foreground fg-light))
   `(org-habit-ready-face ,(sendai-face :background green-mid
                                        :foreground fg-light))
   `(org-habit-ready-future-face ,(sendai-face :background green-primary
                                               :foreground fg-light))
   `(org-habit-alert-face ,(sendai-face :background yellow-dark
                                        :foreground fg-light))
   `(org-habit-alert-future-face ,(sendai-face :background yellow-mid
                                               :foreground fg-light))
   `(org-habit-overdue-face ,(sendai-face :background red-mid
                                          :foreground fg-light))
   `(org-habit-overdue-future-face ,(sendai-face :background red-primary
                                                 :foreground fg-light))

   ;; outline-mode
   `(outline-1 ,(sendai-face :foreground yellow-primary :weight 'bold))
   `(outline-2 ,(sendai-face :foreground cyan-light :weight 'bold))
   `(outline-3 ,(sendai-face :foreground violet-light :weight 'bold))
   `(outline-4 ,(sendai-face :foreground blue-light :weight 'bold))
   `(outline-5 ,(sendai-face :foreground yellow-light :weight 'bold))
   `(outline-6 ,(sendai-face :foreground cyan-light :weight 'bold))
   `(outline-7 ,(sendai-face :foreground violet-light :weight 'bold))
   `(outline-8 ,(sendai-face :foreground blue-light :weight 'bold))

   ;; package
   `(package-status-built-in ,(sendai-face :foreground blue-light))
   `(package-status-installed ,(sendai-face :foreground green-light))
   `(package-status-from-source ,(sendai-face :foreground cyan-primary))
   `(package-status-dependency ,(sendai-face :foreground yellow-light))
   `(package-status-held ,(sendai-face :foreground violet-light))
   `(package-status-disabled ,(sendai-face :foreground fg-darker))
   `(package-status-new ,(sendai-face :foreground fg-light :weight 'bold))
   `(package-status-avail-obso ,(sendai-face :foreground fg-darker))
   `(package-status-unsigned
     ,(sendai-face :underline `(:color ,red-primary :style wave)))

   ;; proced
   `(proced-user ,(sendai-face :weight 'bold))
   `(proced-emacs-pid ,(sendai-face :foreground violet-primary))
   `(proced-pid ,(sendai-face :foreground blue-primary))
   `(proced-session-leader-pid ,(sendai-face :foreground blue-primary
                                             :underline t))
   `(proced-ppid ,(sendai-face :foreground blue-mid))
   `(proced-pgrp ,(sendai-face :foreground blue-mid))
   `(proced-sess ,(sendai-face :foreground blue-mid))
   `(proced-cpu ,(sendai-face :foreground cyan-primary :weight 'bold))
   `(proced-mem ,(sendai-face :foreground cyan-mid))
   `(proced-memory-low-usage ,(sendai-face :foreground green-primary))
   `(proced-memory-medium-usage ,(sendai-face :foreground yellow-primary))
   `(proced-memory-high-usage ,(sendai-face :foreground orange-primary))
   `(proced-run-status-code ,(sendai-face :foreground green-light))
   `(proced-interruptible-sleep-status-code
     ,(sendai-face :foreground bg-lighter))
   `(proced-uninterruptible-sleep-status-code
     ,(sendai-face :foreground red-primary))
   `(proced-time-colon ,(sendai-face :foreground fg-darker))
   `(proced-executable ,(sendai-face :foreground blue-light))
   `(proced-mark ,(sendai-face :foreground yellow-primary :weight 'bold))
   `(proced-marked ,(sendai-face :inherit 'proced-mark))
   `(proced-sort-header ,(sendai-face :foreground fg-dark :inverse-video t
                                      :weight 'bold))

   ;; ruler-mode
   `(ruler-mode-default
     ,(sendai-face :background bg-light :foreground fg-darker
                    :box `(:line-width 1 :color ,bg-light
                           :style released-button)))
   `(ruler-mode-column-number ,(sendai-face :foreground fg-primary
                                            :inherit 'ruler-mode-default))
   `(ruler-mode-current-column ,(sendai-face :foreground yellow-primary
                                             :weight 'bold
                                             :inherit 'ruler-mode-default))
   `(ruler-mode-comment-column ,(sendai-face :foreground cyan-light
                                             :inherit 'ruler-mode-default))
   `(ruler-mode-fill-column ,(sendai-face :foreground cyan-light
                                          :inherit 'ruler-mode-default))
   `(ruler-mode-goal-column ,(sendai-face :foreground cyan-light
                                          :inherit 'ruler-mode-default))
   `(ruler-mode-tab-stop ,(sendai-face :foreground blue-primary
                                       :inherit 'ruler-mode-default))
   `(ruler-mode-fringes ,(sendai-face :inherit 'ruler-mode-default))

   ;; sh-mode
   `(sh-escaped-newline ,(sendai-face :foreground fg-darker))
   `(sh-heredoc ,(sendai-face :foreground green-light))
   `(sh-quoted-exec ,(sendai-face :foreground orange-light))

   ;; shr
   `(shr-mark ,(sendai-face :background yellow-primary :foreground bg-primary))
   `(shr-selected-link
     ,(sendai-face :background cyan-primary :foreground fg-light
                   :box (when (>= emacs-major-version 28)
                          `(:line-width (-1 . -1) :color ,cyan-light))))

   ;; smerge-mode
   `(smerge-markers ,(sendai-face :background blue-darker :foreground fg-light))
   `(smerge-upper ,(sendai-face :background red-darker))
   `(smerge-lower ,(sendai-face :background green-darker))
   `(smerge-base ,(sendai-face :background yellow-darker))
   `(smerge-refined-removed ,(sendai-face :background red-dark))
   `(smerge-refined-added ,(sendai-face :background green-dark))

   ;; tab-bar
   `(tab-bar ,(sendai-face :background bg-lighter :inherit 'variable-pitch))
   `(tab-bar-tab-group-current ,(sendai-face :weight 'bold))
   `(tab-bar-tab-group-inactive ,(sendai-face :inherit 'tab-bar-tab-inactive
                                              :weight 'bold))
   `(tab-bar-tab
     ,(sendai-face :background bg-primary :foreground fg-primary
                   :overline yellow-primary
                   :box `(:line-width ,sendai-tab-padding :color ,bg-primary)))
   `(tab-bar-tab-inactive
     ,(sendai-face :background bg-light :foreground fg-dark
                   :box `(:line-width ,sendai-tab-padding :color ,bg-light)))
   `(tab-bar-tab-ungrouped ,(sendai-face :inherit 'tab-bar-tab-inactive))

   ;; tab-line
   `(tab-line ,(sendai-face :background bg-lighter :inherit 'variable-pitch))
   `(tab-line-tab-group ,(sendai-face :inherit 'tab-line :weight 'bold))
   `(tab-line-tab
     ,(sendai-face :background bg-primary :foreground fg-darker
                   :overline yellow-dark
                   :box `(:line-width ,sendai-tab-padding :color ,bg-primary)))
   `(tab-line-tab-current
     ,(sendai-face :background bg-primary :foreground fg-primary
                   :overline yellow-primary
                   :box `(:line-width ,sendai-tab-padding :color ,bg-primary)))
   `(tab-line-tab-inactive
     ,(sendai-face :background bg-light :foreground fg-dark
                   :overline bg-light
                   :box `(:line-width ,sendai-tab-padding :color ,bg-light)))
   `(tab-line-tab-inactive-alternate ,(sendai-face :overline blue-mid))
   `(tab-line-highlight ,(sendai-face :foreground fg-light))
   `(tab-line-tab-special ,(sendai-face :slant 'italic))

   ;; vc-dir
   `(vc-dir-header ,(sendai-face :foreground blue-light))
   `(vc-dir-header-value ,(sendai-face :foreground fg-primary))
   `(vc-dir-directory ,(sendai-face :foreground blue-primary :weight 'bold))
   `(vc-dir-file ,(sendai-face :foreground fg-primary))
   `(vc-dir-status-up-to-date ,(sendai-face :foreground green-light))
   `(vc-dir-status-edited ,(sendai-face :foreground yellow-light))
   `(vc-dir-status-ignored ,(sendai-face :foreground fg-darker))
   `(vc-dir-status-warning ,(sendai-face :foreground red-primary :weight 'bold))
   `(vc-dir-mark-indicator ,(sendai-face :foreground yellow-primary
                                         :weight 'bold))

   ;; whitespace-mode
   `(whitespace-space ,(sendai-face :foreground violet-primary))
   `(whitespace-hspace ,(sendai-face :inherit 'whitespace-space))
   `(whitespace-tab ,(sendai-face :inherit 'whitespace-space))
   `(whitespace-newline ,(sendai-face :foreground bg-lighter))
   `(whitespace-indentation ,(sendai-face :background violet-dark
                                          :foreground fg-primary))
   `(whitespace-empty ,(sendai-face :inherit 'whitespace-indentation))
   `(whitespace-big-indent ,(sendai-face :background violet-primary
                                         :foreground fg-light))
   `(whitespace-line ,(sendai-face :background red-dark))
   `(whitespace-trailing ,(sendai-face :background red-primary
                                       :foreground fg-light))
   `(whitespace-missing-newline-at-eof
     ,(sendai-face :inherit 'whitespace-trailing))
   `(whitespace-space-before-tab ,(sendai-face :inherit 'whitespace-trailing))
   `(whitespace-space-after-tab ,(sendai-face :inherit 'whitespace-indentation))

   ;; --------------------
   ;; Third-party packages
   ;; --------------------

   ;; company-mode
   `(company-preview ,(sendai-face :foreground fg-primary
                                    :background blue-dark))
   `(company-preview-common ,(sendai-face :foreground yellow-primary))
   `(company-preview-search ,(sendai-face :inherit 'isearch))
   `(company-template-field ,(sendai-face :foreground fg-primary
                                          :background bg-lighter))
   `(company-tooltip ,(sendai-face :foreground fg-primary
                                   :background bg-lighter))
   `(company-tooltip-common ,(sendai-face :foreground yellow-primary))
   `(company-tooltip-annotation ,(sendai-face :foreground cyan-light))
   `(company-tooltip-selection ,(sendai-face :background blue-mid))
   `(company-tooltip-search ,(sendai-face :inherit 'isearch))
   `(company-tooltip-search-selection ,(sendai-face :inherit 'isearch))
   `(company-scrollbar-fg ,(sendai-face :background fg-darker))
   `(company-scrollbar-bg ,(sendai-face :background bg-light))
   `(company-echo-common ,(sendai-face :foreground yellow-primary))

   ;; corfu-mode
   `(corfu-default ,(sendai-face :foreground fg-primary :background bg-lighter))
   `(corfu-border ,(sendai-face :background bg-darker))
   `(corfu-bar ,(sendai-face :background fg-darker))
   `(corfu-current ,(sendai-face :background blue-mid))

   ;; elfeed
   `(elfeed-search-title-face ,(sendai-face :foreground fg-primary))
   `(elfeed-search-unread-title-face ,(sendai-face :weight 'bold
                                                   :slant 'italic))
   `(elfeed-search-date-face ,(sendai-face :foreground fg-darker))
   `(elfeed-search-feed-face ,(sendai-face :foreground blue-primary))
   `(elfeed-search-tag-face ,(sendai-face :foreground yellow-light))
   `(elfeed-search-unread-count-face ,(sendai-face :foreground blue-light))
   `(elfeed-log-date-face ,(sendai-face :foreground cyan-primary))
   `(elfeed-log-error-level-face ,(sendai-face :foreground red-primary))
   `(elfeed-log-warn-level-face ,(sendai-face :foreground yellow-primary))
   `(elfeed-log-info-level-face ,(sendai-face :foreground blue-primary))
   `(elfeed-log-debug-level-face ,(sendai-face :foreground magenta-light))

   ;; form-feed
   `(form-feed-line ,(sendai-make-face
                      `(((supports :strike-through t))
                        :foreground ,fg-darker :strike-through t)
                      `(nil :foreground ,fg-darker :underline t)))

   ;; hl-todo
   `(hl-todo ,(sendai-face :foreground yellow-light :weight 'bold))
   `(sendai-hl-todo-error ,(sendai-face :inherit 'hl-todo
                                        :foreground red-primary))
   `(sendai-hl-todo-warning ,(sendai-face :inherit 'hl-todo
                                          :foreground orange-primary))
   `(sendai-hl-todo-success ,(sendai-face :inherit 'hl-todo
                                          :foreground green-primary))

   ;; js2-mode
   `(js2-external-variable ,(sendai-face :foreground orange-primary))
   `(js2-function-param ,(sendai-face :foreground yellow-light))
   `(js2-jsdoc-tag ,(sendai-face :inherit 'c-annotation-face))
   `(js2-jsdoc-type ,(sendai-face :inherit 'font-lock-type-face))
   `(js2-jsdoc-value ,(sendai-face :inherit 'font-lock-variable-name-face))
   `(js2-jsdoc-html-tag-delimiter ,(sendai-face :foreground fg-primary))
   `(js2-jsdoc-html-tag-name ,(sendai-face :foreground yellow-primary))
   `(js2-error ,(sendai-face :foreground red-primary))
   `(js2-warning
     ,(sendai-face :underline `(:color ,orange-primary :style wave)))

   ;; markdown-mode
   `(markdown-header-face ,(sendai-face :underline t))
   `(markdown-header-face-1
     ,(sendai-face :inherit '(outline-1 markdown-header-face)))
   `(markdown-header-face-2
     ,(sendai-face :inherit '(outline-2 markdown-header-face)))
   `(markdown-header-face-3
     ,(sendai-face :inherit '(outline-3 markdown-header-face)))
   `(markdown-header-face-4
     ,(sendai-face :inherit '(outline-4 markdown-header-face)))
   `(markdown-header-face-5
     ,(sendai-face :inherit '(outline-5 markdown-header-face)))
   `(markdown-header-face-6
     ,(sendai-face :inherit '(outline-6 markdown-header-face)))
   `(markdown-html-tag-delimiter-face ,(sendai-face :inherit 'default))
   `(markdown-html-tag-name-face
     ,(sendai-face :inherit 'font-lock-function-name-face))
   `(markdown-html-attr-name-face
     ,(sendai-face :inherit 'font-lock-variable-name-face))
   `(markdown-html-attr-value-face
     ,(sendai-face :inherit 'font-lock-string-face))
   `(markdown-html-entity-face
     ,(sendai-face :inherit 'font-lock-variable-name-face))
   `(markdown-gfm-checkbox-face ,(sendai-face :foreground fg-darker
                                              :weight 'bold))
   `(markdown-table-face ,(sendai-face :foreground blue-light
                                       :background blue-darker))
   `(markdown-inline-code-face ,(sendai-face :foreground cyan-primary))
   `(markdown-pre-face ,(sendai-face :foreground fg-darker))
   `(markdown-code-face ,(sendai-face :background blue-darker :extend t))
   `(markdown-language-keyword-face ,(sendai-face :foreground cyan-light))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ,(sendai-face :foreground fg-primary))
   `(rainbow-delimiters-depth-2-face ,(sendai-face :foreground fg-darker))
   `(rainbow-delimiters-depth-3-face ,(sendai-face :foreground fg-light))
   `(rainbow-delimiters-depth-4-face ,(sendai-face :foreground fg-dark))
   `(rainbow-delimiters-base-error-face ,(sendai-face :foreground red-primary))

   ;; solaire-mode
   `(solaire-default-face ,(sendai-face :background bg-dark
                                        :foreground fg-primary))
   `(solaire-fringe-face ,(sendai-face :background bg-dark))

   ;; telephone-line
   `(telephone-line-accent-active ,(sendai-face :background blue-mid))
   `(telephone-line-accent-inactive ,(sendai-face :background blue-dark))

   ;; wgrep
   `(wgrep-face ,(sendai-face :background violet-dark))
   `(wgrep-done-face ,(sendai-face :foreground green-light
                                   :distant-foreground green-dark))
   `(wgrep-delete-face ,(sendai-face :background red-darker
                                     :foreground fg-dark))
   `(wgrep-reject-face ,(sendai-face :weight 'bold :foreground red-primary))
   `(wgrep-file-face ,(sendai-face :background bg-light))

   ;; which-key
   `(which-key-key-face ,(sendai-face :foreground yellow-primary))
   `(which-key-command-description-face ,(sendai-face :foreground blue-light))
   `(which-key-group-description-face ,(sendai-face :foreground cyan-primary
                                                    :weight 'bold))

   ;; yaml-mode
   `(yaml-tab-face ,(sendai-face :background red-primary)))

  (when (< emacs-major-version 28)
    (custom-theme-set-faces
     'sendai
     ;; Backwards compatibility for term
     `(term-color-black ,(sendai-face :background bg-lighter
                                      :foreground bg-lighter))
     `(term-color-red ,(sendai-face :background red-light
                                    :foreground red-light))
     `(term-color-green ,(sendai-face :background green-light
                                      :foreground green-light))
     `(term-color-yellow ,(sendai-face :background yellow-light
                                       :foreground yellow-light))
     `(term-color-blue ,(sendai-face :background blue-primary
                                     :foreground blue-primary))
     `(term-color-magenta ,(sendai-face :background magenta-primary
                                        :foreground magenta-primary))
     `(term-color-cyan ,(sendai-face :background cyan-primary
                                     :foreground cyan-primary))
     `(term-color-white ,(sendai-face :background fg-primary
                                      :foreground fg-primary))))

  (custom-theme-set-variables
   'sendai
   `(hl-todo-keyword-faces
     '(("FIXME" . sendai-hl-todo-error)
       ("TODO"  . sendai-hl-todo-warning)
       ("XXX"   . sendai-hl-todo-warning)
       ("DONE"  . sendai-hl-todo-success))))

  (when-let ((class-name (sendai-active-class)))
    (when (< emacs-major-version 28)
      (custom-theme-set-variables
       'sendai
       `(ansi-color-names-vector
         ,(sendai-subst
           (vector bg-lighter red-light green-light yellow-light
                   blue-primary magenta-primary cyan-primary fg-primary)
           class-name))))

    (custom-theme-set-variables
     'sendai
     `(rainbow-delimiters-max-face-count 4)

     `(xterm-color-names
       ,(sendai-subst
         (vector bg-lighter red-light green-light yellow-light
                 blue-primary magenta-primary cyan-primary fg-primary)
         class-name))
     `(xterm-color-names-bright
       ,(sendai-subst
         (vector fg-darker red-primary green-primary yellow-primary
                 blue-light magenta-light cyan-light fg-light)
         class-name)))))

(provide-theme 'sendai)

;;; sendai-theme.el ends here
