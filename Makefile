OCAMLC = ocamlfind ocamlc
OCAMLMKTOP = ocamlfind ocamlmktop

OCAMLFLAGS = -package batteries -linkpkg -g

SOURCES = mystd.ml tree.ml modelcheck.ml main.ml

.PHONY: clean

a.out: $(SOURCES)
	$(OCAMLC) $(OCAMLFLAGS) -o a.out $(SOURCES)

top: $(SOURCES)
	$(OCAMLMKTOP) $(OCAMLFLAGS) -o top $(SOURCES)

clean:
	$(RM) *.cmo *.cmi a.out top