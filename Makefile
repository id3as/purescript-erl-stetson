.PHONY: all clean test

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)


all: src/compiled_ps docs

src/compiled_ps: output/.complete
	rm -f $$PWD/src/compiled_ps
	ln -s $$PWD/output $$PWD/src/compiled_ps

output/.complete: $(PS_SOURCEFILES) $(PS_ERL_FFI) .spago
	echo Stuff updated, running spago
	spago build && touch output/.complete
	spago build --config test.dhall

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
	rm -rf $(OUTPUT)/*

testbuild: test.dhall
	spago build --config test.dhall

test: testbuild erl
	erl -pa ebin -pa cowboy/ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl