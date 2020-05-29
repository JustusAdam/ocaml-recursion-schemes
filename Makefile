MODULES=recursion_schemes

rec-scm.cmx: $(addsuffix .ml,$(MODULES))
	ocamlopt -a $^ -o $@

.PHONY: clean format

format: $(addsuffix .ml,$(MODULES))
	ocamlformat -i $^

clean::
	rm -rf *.a *.cmi *.o *.cmx
