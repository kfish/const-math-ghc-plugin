OPTS=-O2 -fforce-recomp -fplugin ConstMath.Plugin Sqrt.hs

all:
	ghc $(OPTS)

core:
	ghc-core -- $(OPTS)

clean:
	rm -f *.o *.hi Sqrt
