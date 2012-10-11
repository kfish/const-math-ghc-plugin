GHC=ghc

NOPLUGIN_OPTS=-O2 -fforce-recomp
#WITHPLUGIN_OPTS=$(NOPLUGIN_OPTS) -fplugin ConstMath.Plugin -fplugin-opt=ConstMath.Plugin:--trace
WITHPLUGIN_OPTS=$(NOPLUGIN_OPTS) -fplugin ConstMath.Plugin

ERROR_TESTS= \
	1603.hs \
	3676.hs \
	4381.hs \
	4383.hs \
	arith001.hs \
	arith002.hs \
	arith003.hs \
	arith004.hs \
	arith005.hs \
	arith006.hs \
	arith007.hs \
	arith008.hs \
	arith009.hs \
	arith010.hs \
	arith011.hs \
	arith012.hs \
	arith013.hs \
	arith014.hs \
	arith015.hs \
	arith016.hs \
	arith017.hs \
	arith018.hs \
	arith019.hs \
	expfloat.hs \
	numrun009.hs \
	numrun010.hs \
	numrun011.hs \
	numrun012.hs \
	numrun013.hs \
	numrun014.hs

check.%.hs: %.hs
	@echo "Checking $< ..."
	$(GHC) $(NOPLUGIN_OPTS) $< -o $*.noplugin.exe
	./$*.noplugin.exe 1>$*.noplugin.stdout 2>$*.noplugin.stderr
	$(GHC) $(WITHPLUGIN_OPTS) $< -o $*.withplugin.exe
	./$*.withplugin.exe 1>$*.withplugin.stdout 2>$*.withplugin.stderr
	@diff -u $*.noplugin.stdout $*.withplugin.stdout
	@diff -u $*.noplugin.stderr $*.withplugin.stderr
	-rm -f $*.noplugin.exe $*.withplugin.exe $*.noplugin.stdout $*.noplugin.stderr  $*.withplugin.stdout $*.withplugin.stderr

all :: $(patsubst %, check.%, $(ERROR_TESTS))

clean:
	rm -f *.o *.hi *.exe *.stdout *.stderr
