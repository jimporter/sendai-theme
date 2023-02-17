EMACS ?= emacs
EMACS_SRC ?= ~/src/emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l sendai-theme -l sendai-tests \
	--eval '(ert-run-tests-batch-and-exit t)'

SAMPLE_FILES = \
  $(PWD)/README.org \
  $(PWD)/sendai-theme.el \
  $(EMACS_SRC)/src/menu.c \
  $(EMACS_SRC)/autogen.sh

define SCRIPT
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-width (selected-frame) 160)
  (set-frame-height (selected-frame) 64)
  (add-to-list 'custom-theme-load-path default-directory)
  (load-theme 'sendai t)
  (add-hook 'change-major-mode-hook #'elide-head)
  (setq-default enable-local-variables nil)
  (find-file (pop command-line-args-left))
  (ignore-errors (scroll-up-command))
  (split-window-below)
  (split-window-right)
  (other-window 1)
  (find-file (pop command-line-args-left))
  (ignore-errors (scroll-up-command))
  (other-window 1)
  (find-file (pop command-line-args-left))
  (ignore-errors (scroll-up-command))
  (split-window-right)
  (other-window 1)
  (find-file (pop command-line-args-left))
  (ignore-errors (scroll-up-command))
  (sit-for 4)
  (kill-emacs))
endef
export SCRIPT

.PHONY: screenshot
screenshot: screenshot.png

screenshot.png: FORCE
	scrot --delay 4 --focused -o $@ &
	$(EMACS) -Q --eval "$$SCRIPT" $(SAMPLE_FILES)

FORCE:
