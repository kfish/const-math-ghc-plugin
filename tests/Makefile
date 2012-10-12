GHC=ghc

GHC_MAJMIN=`$(GHC) --numeric-version | cut -d. -f1,2)`

all:
	$(MAKE) -C ghc-$(GHC_MAJMIN)

clean:
	$(MAKE) -C ghc-$(GHC_MAJMIN) clean
	rm -f *.o *.hi *.exe *.stdout *.stderr
