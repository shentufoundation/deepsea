cppo_defs := # "cppo_D(REDACTED)" # Quotes are necessary
cppo_flags = -use-ocamlfind -plugin-tag "package(cppo_ocamlbuild)" -tag $(cppo_defs)

.PHONY: edsger
edsger:
	ocamlbuild -libs str -pkg cryptokit -I Edsger -I backend edsger.byte -use-menhir $(cppo_flags)
	cp _build/Edsger/edsger.byte dsc

.PHONY: edsger.native
edsger.native:
	ocamlbuild -libs str -pkg cryptokit -I Edsger -I backend edsger.native -use-menhir $(cppo_flags)
	cp _build/Edsger/edsger.native dsc
