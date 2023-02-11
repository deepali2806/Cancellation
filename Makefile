OCAMLPATH=~/repos/ocaml-multicore/_install/bin/

all:
	ocamlfind ocamlopt -o test_fifo.exe -linkpkg -package cancel_unified_interface,eio,eio_main,eio_linux,eio_luv -thread fifo.mli fifo.ml test_cancel.ml

clean:
	rm -f *~ *.cm* *.o *.out *.exe
