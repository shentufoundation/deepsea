core: core.make
	make -f core.make

clean: core.make
	make -f core.make clean
	rm -r _build

core.make: _CoqProject
	coq_makefile -f _CoqProject -o core.make

.PHONY: clean core
