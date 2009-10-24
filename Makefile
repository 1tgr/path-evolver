all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -threaded -o bin/path-evolver -outputdir obj -iHUnit-1.0 *.hs

clean:
	rm -r bin obj

test: build
	bin/path-evolver +RTS -N4
