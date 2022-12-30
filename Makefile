EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l sendai-theme -l sendai-tests \
	--eval '(ert-run-tests-batch-and-exit t)'
