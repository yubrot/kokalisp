.PHONY: test

SRCDIRS := kokalisp kokalisp/driver kokalisp/sexp kokalisp/support kokalisp/vm
SOURCES := $(foreach dir,$(SRCDIRS),$(wildcard $(dir)/*.kk)) kokalisp/driver/bootcode.kk

all: kklisp

# FIXME: remove chmod after https://github.com/koka-lang/koka/issues/322 fixed
kklisp: $(SOURCES)
	koka -O2 -o kklisp kokalisp/driver/main.kk
	chmod +x kklisp

test: $(SOURCES)
	koka -e kokalisp/driver/main.kk -- -test rosetta-lisp/test

kokalisp/driver/bootcode.kk: rosetta-lisp/boot.lisp
	koka -e kokalisp/driver/gen-bootcode.kk -- $@ $<
