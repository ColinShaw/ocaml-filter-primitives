stuff:
	ocamlopt -S -inline 1000 -o norm norms.ml

clean:
	rm -f *.cmi *.cmx *.o *.s norm
