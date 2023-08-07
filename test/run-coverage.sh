#!/bin/bash

sbcl	--noinform \
	--disable-ldb \
	--lose-on-corruption \
	--end-runtime-options \
	--no-sysinit \
	--no-userinit \
	--disable-debugger \
	--eval '(require :asdf)' \
	--load 'code-coverage.lisp' \
	--quit \
	--end-toplevel-options
