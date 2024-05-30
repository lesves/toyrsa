.PHONY: clean all run

all: Main

run: all
	./Main

clean:
	rm -f Main
	rm -f *.hi *.o **/*.hi **/*.o

Main: Main.hs ToyRSA/Primes.hs ToyRSA/Utils.hs ToyRSA/RSA.hs
	ghc Main.hs
