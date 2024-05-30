.PHONY: clean all run

all: toyrsa

run: all
	./toyrsa

clean:
	rm -f toyrsa
	rm -f *.hi *.o **/*.hi **/*.o

toyrsa: Main.hs Lib/Primes.hs Lib/Utils.hs Lib/RSA.hs
	ghc Main.hs -o toyrsa
