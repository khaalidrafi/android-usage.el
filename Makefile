EMACS ?= emacs

LOAD = -L .

SRCS = android-usage.el

all: compile

compile:
	$(EMACS) --batch $(LOAD) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS)

test:
	$(EMACS) --batch $(LOAD) -l test -f ert-run-tests-batch-and-exit

clean:
	rm -f $(SRCS:.el=.elc) test.elc

.PHONY: all compile test clean
