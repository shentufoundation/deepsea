This source directory contains the OCaml parts of the DeepSEA compiler. For the
time being, we do not include the sources for the parts that are written in
Coq, but we ship the Ocaml files which were compiled from them.

In order to build it, use opam to install the prerequisites, and then run make:

```
opam install .
make edsger
mv _build/default/Edsger/edsger.bc dsc
```
