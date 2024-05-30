.PHONY: clean all run

all: Main

run: all
	./Main

clean:
	rm Main
	rm *.hi
	rm *.o

Main: Main.hs Primes.hs Utils.hs
	ghc Main.hs
