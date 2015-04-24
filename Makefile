all: main

FILES = Order.ml Dict.ml Graph.ml Heap.ml Main.ml

main: $(FILES)
	corebuild main.native

clean:
	rm -rf _build main.native
