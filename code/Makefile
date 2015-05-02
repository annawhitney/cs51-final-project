all: Heap

FILES = Order.ml Dict.ml Graph.ml Links.ml Distance.ml Heap.ml

Heap: $(FILES)
	ocamlbuild -use-ocamlfind -package re2 -package core -tag thread Heap.native

clean:
	rm -rf _build Heap.native
