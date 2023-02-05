OCAMLPATH=~/repos/ocaml-multicore/_install/bin/

all:
	ocamlfind ocamlopt -o test_fifo.exe -linkpkg -package cancel_unified_interface -thread fifo.mli fifo.ml test.ml

clean:
	rm -f *~ *.cm* *.o *.out *.exe
