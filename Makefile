all: main

FILES = Order.ml Dict.ml Graph.ml Links.ml Heap.ml Main.ml

main: $(FILES)
	corebuild Main.native

clean:
	rm -rf _build Main.native
