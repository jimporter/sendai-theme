# Copyright (C) 2021-2026 Free Software Foundation, Inc.

# This file is NOT part of GNU Emacs.

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

PACKAGE_NAME := sendai-theme
PACKAGE_MAIN := $(PACKAGE_NAME).el
AUTOLOADS := $(PACKAGE_NAME)-autoloads.el
PKG_FILE := $(PACKAGE_NAME)-pkg.el
TESTS := $(wildcard *-tests.el)
TEST_OBJS := $(patsubst %.el,%.elc,$(TESTS))
SRCS := $(filter-out $(AUTOLOADS) $(PKG_FILE) $(TESTS), $(wildcard *.el))
OBJS := $(patsubst %.el,%.elc,$(SRCS))

EMACS ?= emacs
EMACS_SRC ?= ~/src/emacs
export EMACS_SRC

define DOC_EXPORT_SCRIPT
(progn
  (setq org-element-cache-persistent nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-html-link-use-abs-url t
        org-html-link-home (getenv "DOC_BASE_URL"))
  (setq gc-cons-threshold (* 50 1000 1000))
  (require (quote ox-html))
  (org-html-export-to-html nil nil nil t))
endef
export DOC_BASE_URL
export DOC_EXPORT_SCRIPT

.PHONY: all
all: compile autoloads

.PHONY: compile
compile: $(OBJS)

.PHONY: compile-tests
compile-tests: $(TEST_OBJS)

.PHONY: autoloads
autoloads: $(AUTOLOADS)

$(AUTOLOADS): GENERATE_AUTOLOADS = '$\
  (package-generate-autoloads "$(PACKAGE_NAME)" default-directory)'
$(AUTOLOADS): $(SRCS)
	@echo AUTOLOAD $@
	@$(EMACS) -Q --batch \
	  --eval '(package-initialize)' \
	  --eval $(GENERATE_AUTOLOADS)

%.elc: %.el
	@echo ELC $@
	@$(EMACS) -Q --batch \
	  $(if $(STRICT),--eval '(setq byte-compile-error-on-warn t)') \
	  -L . --funcall batch-byte-compile $<

.PHONY: screenshot
screenshot: etc/screenshot.png

etc/screenshot.png: FORCE
	scrot --delay 4 --focused -o $@ &
	$(EMACS) -Q -L . -l etc/sendai-demo \
	  --eval '(progn (sit-for 4) (kill-emacs))'

README.html: README.org
	@echo EXPORT $@
	@$(EMACS) -Q --batch \
	  --find-file $< --eval "$$DOC_EXPORT_SCRIPT"

.PHONY: run
run: all
	$(EMACS) -Q -L . \
	  --eval '(progn (load "$(AUTOLOADS)") (load-theme '\''sendai t))'

.PHONY: lint
lint:
	@$(MAKE) --always-make STRICT=1 compile compile-tests

.PHONY: check
check: $(if $(NO_COMPILE),,$(OBJS) $(TEST_OBJS))
	@echo TEST $(patsubst %.el,%,$(TESTS))
	@$(EMACS) -Q --batch \
	  -L . $(patsubst %.el,-l %,$(TESTS)) \
	  --eval '(ert-run-tests-batch-and-exit t)'

.PHONY: clean
clean:
	rm -f *.elc $(AUTOLOADS) $(PKG_FILE) README.html

FORCE:
