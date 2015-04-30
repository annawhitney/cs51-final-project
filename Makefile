all: main

FILES = Order.ml Dict.ml Graph.ml Links.ml Heap.ml Main.ml test.ml testread.ml 

main: $(FILES)
	corebuild Main.native

clean:
	rm -rf _build Main.native
