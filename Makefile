.PHONY: all clean test

PS_SRC = src
TEST_SRC = test
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
PS_TEST_SOURCEFILES = $(shell find ${TEST_SRC} -type f -name \*.purs)
PS_TEST_ERL_FFI = $(shell find ${TEST_SRC} -type f -name \*.erl)


all: src/compiled_ps erl test

src/compiled_ps: output/.complete
	rm -f $$PWD/src/compiled_ps
	ln -s $$PWD/output $$PWD/src/compiled_ps

output/.complete: $(PS_SOURCEFILES) $(PS_ERL_FFI) $(PS_TEST_SOURCEFILES) $(PS_TEST_ERL_FFI) .spago
	echo Stuff updated, running spago
	spago build && touch output/.complete

docs: $(PS_SOURCEFILES) $(PS_ERL_FFI)
	mkdir -p docs
	purs docs '$(PS_SRC)/**/*.purs' \
		--docgen Stetson:docs/Stetson.md \
		--docgen Stetson.Rest:docs/Stetson.Rest.md
	touch docs

.spago: spago.dhall packages.dhall
	spago install
	touch .spago

clean:
	rm -rf $(OUTPUT)/* $(OUTPUT)/.complete
	make -C test clean

testbuild: all
	make -C test testbuild

test: testbuild
	make -C test test

erl: output/.complete
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

