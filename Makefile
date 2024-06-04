all: kklisp

# FIXME: remove chmod after https://github.com/koka-lang/koka/issues/322 fixed
kklisp:
	koka -O2 -o kklisp kokalisp/main.kk
	chmod +x kklisp

test:
	koka -e kokalisp/main.kk -- -test rosetta-lisp/test
