all: clean graphc.native inbuilt.o

inbuilt : inbuilt.c
	cc -o inbuilt inbuilt.c

graphc.native:
	ocamlbuild -use-ocamlfind -pkgs llvm graphc.native

clean:

	rm -rf *.o *.s *.ll *.out *.exe *.native *.cmi *.cmo *.cmx  ./_build  

