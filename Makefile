EMACS    ?= emacs
EMACSCMD := $(EMACS) -Q --batch -L lisp -L tests

.PHONY: test test-unit test-transport test-files clean

test:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-all)'

test-unit:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 0)'

test-transport:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 1)'

test-files:
	$(EMACSCMD) -l tests/run-all.el --eval '(angelia-tests-run-layer 2)'

clean:
	find lisp tests -name '*.elc' -delete
	rm -rf tests/tmp
