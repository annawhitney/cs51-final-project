all: main

FILES = Heap.ml dict.ml  main.ml

main: $(FILES)
	corebuild main.native

clean:
	rm -rf _build main.native
