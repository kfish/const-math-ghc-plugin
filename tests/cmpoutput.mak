GHC=ghc

NOPLUGIN_OPTS=-O2 -fforce-recomp
#WITHPLUGIN_OPTS=$(NOPLUGIN_OPTS) -fplugin ConstMath.Plugin -fplugin-opt=ConstMath.Plugin:--trace
WITHPLUGIN_OPTS=$(NOPLUGIN_OPTS) -fplugin ConstMath.Plugin

check.%.hs: %.hs
	@echo "Checking $< ..."
	$(GHC) $(NOPLUGIN_OPTS) $< -o $*.noplugin.exe
	./$*.noplugin.exe 1>$*.noplugin.stdout 2>$*.noplugin.stderr
	$(GHC) $(WITHPLUGIN_OPTS) $< -o $*.withplugin.exe
	./$*.withplugin.exe 1>$*.withplugin.stdout 2>$*.withplugin.stderr
	@diff -u $*.noplugin.stdout $*.withplugin.stdout
	@diff -u $*.noplugin.stderr $*.withplugin.stderr
	-rm -f $*.noplugin.exe $*.withplugin.exe $*.noplugin.stdout $*.noplugin.stderr  $*.withplugin.stdout $*.withplugin.stderr
