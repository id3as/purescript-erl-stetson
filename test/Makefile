.PHONY: all clean test

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)

ifeq (, $(shell which rebar3))
 $(error "rebar3 is not available, tests cannot be run")
endif

all: src/compiled_ps

clean:
	rm -rf $(OUTPUT)/*
	rm -f _build/default/lib/*/ebin/*.beam

testbuild:
	ln -sf ../../src/compiled_ps/ src/
	rebar3 compile

test: testbuild
	erl -pa ebin -pa _build/default/lib/*/ebin -noshell -eval '(stetson_test_main@ps:main())()' -eval 'init:stop()'

