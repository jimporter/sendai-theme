EMACS ?= emacs
EMACS_SRC ?= ~/src/emacs
export EMACS_SRC

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l sendai-theme -l sendai-tests \
	--eval '(ert-run-tests-batch-and-exit t)'

.PHONY: screenshot
screenshot: etc/screenshot.png

etc/screenshot.png: FORCE
	scrot --delay 4 --focused -o $@ &
	$(EMACS) -Q -L . -l etc/sendai-demo \
	--eval '(progn (sit-for 4) (kill-emacs))'

FORCE:
