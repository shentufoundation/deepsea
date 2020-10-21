.PHONY: opensc

all: opensc

opensc:
	ocamlbuild -pkg cryptokit -I backend opensc.native

clean:
	rm -f *.cmo *.cmi *.native