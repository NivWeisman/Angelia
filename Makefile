EMACS    ?= emacs
EMACSCMD := $(EMACS) -Q --batch -L lisp -L tests

.PHONY: test test-unit test-transport test-files compare clean

test:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-all)'

# Benchmark + correctness comparison against TRAMP (file ops + dired).
# Not part of `make test': it is the only thing that uses TRAMP, it is slow
# (real ssh round-trips), and it is meant to be read, not gated on.
compare:
	$(EMACSCMD) -l tests/compare-tramp.el --eval '(angelia-compare-run)'

test-unit:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 0)'

test-transport:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 1)'

test-files:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 2)'

clean:
	find lisp tests -name '*.elc' -delete
	rm -rf tests/tmp
