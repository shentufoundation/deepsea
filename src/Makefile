core: core.make
	make -f core.make

.PHONY: edsger
edsger: Edsger/parser.ml
	cd Edsger && dune build edsger.bc
.PHONY: edsger.native
edsger.exe: Edsger/parser.ml
	cd Edsger && dune build edsger.exe

Edsger/parser.ml: Edsger/config.h Edsger/parser.mly Edsger/make_parser.sh
	Edsger/make_parser.sh

clean: core.make
	make -f core.make clean
	rm -r _build

core.make: _CoqProject
	coq_makefile -f _CoqProject -o core.make

extraction:
	cd backend && coqtop -R .. DeepSpec -top Extract < Extract.v

minicc:
	cd minic && dune build minicc.bc

minicc.exe:
	cd minic && dune build minicc.exe

.PHONY: clean core extraction minicc minicc.exe
