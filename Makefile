.PHONY: all clean test

PS_SRC = src
TEST_SRC = test
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
PS_TEST_SOURCEFILES = $(shell find ${TEST_SRC} -type f -name \*.purs)
PS_TEST_ERL_FFI = $(shell find ${TEST_SRC} -type f -name \*.erl)

all: erl docs

ci: all test

output/.complete: $(PS_SOURCEFILES) $(PS_ERL_FFI) $(PS_TEST_SOURCEFILES) $(PS_TEST_ERL_FFI) .spago
	echo Stuff updated, running spago
	spago build # check the regular spago.dhall is correct, but then build test code
	spago -x test.dhall build && touch output/.complete

docs: $(PS_SOURCEFILES) $(PS_ERL_FFI) output/.complete
	mkdir -p docs
	spago docs --format markdown
	cp generated-docs/md/Stetson*.md docs

.spago: spago.dhall packages.dhall
	spago install
	touch .spago

clean:
	rm -rf $(OUTPUT)/* $(OUTPUT)/.complete
	make -C test clean

cleanspago: clean
	rm -Rf .spago

testbuild: erl
	make -C test testbuild

test: erl
	make -C test test

erl: output/.complete
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

