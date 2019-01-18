.PHONY: all clean test

Y_HOST ?= emacs

Y := Y_HOST="$(Y_HOST)" ./y

MODS := elisp-reader.el \
	y.el

all: $(MODS:.el=.elc)

clean:
	@rm -f *.elc

%.elc : %.el
	@echo "  $@"
	@echo "$<" | Y_LOAD=" " $(Y) -f byte-compile-file

test: all
	@$(Y) --eval "(y-run-tests)"

bench: all
	@$(Y) --eval "(y-run-benchmarks)"
