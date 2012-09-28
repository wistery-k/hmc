OCAMLC = ocamlfind ocamlc
OCAMLMKTOP = ocamlfind ocamlmktop

OCAMLFLAGS = -package batteries -linkpkg -g

.PHONY: clean

a.out: hmc.ml
	$(OCAMLC) $(OCAMLFLAGS) -o a.out hmc.ml

top: hmc.ml
	$(OCAMLMKTOP) $(OCAMLFLAGS) -o top hmc.ml

clean:
	$(RM) *.cmo *.cmi a.out top