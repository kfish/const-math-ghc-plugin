
ERROR_TESTS= $(wildcard *.hs)

include ../cmpoutput.mak

all :: $(patsubst %, check.%, $(ERROR_TESTS))

clean:
	rm -f *.o *.hi *.exe *.stdout *.stderr
