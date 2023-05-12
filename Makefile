# Copyright (C) 2021-2023 Jim Porter

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

EMACS ?= emacs
EMACS_SRC ?= ~/src/emacs
export EMACS_SRC

OBJS := $(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all
all: $(OBJS)

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

.PHONY: lint
lint:
	@$(MAKE) --always-make STRICT=1 all

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l sendai-tests \
	  --eval '(ert-run-tests-batch-and-exit t)'

.PHONY: clean
clean:
	rm -f *.elc

FORCE:
