all: bench-StringMap

bench-%: %.hs en_US.dict force
	ghc -O2 -DTESTING $< -i../$(TOP) -o $@ -outputdir tmp -rtsopts

bench-%.csv: bench-%
	./bench-$* $(BENCHMARK) -v -u bench-$*.csv

.PHONY: force clean veryclean
force:

clean:
	rm -rf tmp $(patsubst %.hs, bench-%, $(wildcard *.hs))
	rm -f en_US.dict

veryclean: clean
	rm -rf *.csv

%.dict:
	aspell -l $* dump master | aspell -l de_DE expand | tr ' ' '\n' > $@
