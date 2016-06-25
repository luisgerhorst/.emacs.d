initNames = init lisp/* lisp/lib/*
initSource = $(foreach name,$(initNames),$(name).el)
initByteCode = $(foreach name,$(initNames),$(name).elc)

build:
	emacs -batch -f batch-byte-compile $(initSource)

clean:
	rm -f $(initByteCode)

.PHONY: build clean
