all: Main

FILES = Order.ml Dict.ml Graph.ml Links.ml Heap.ml Distance.ml Read.ml Main.ml

Main: $(FILES)
	corebuild Main.native

clean:
	rm -rf _build Main.native
