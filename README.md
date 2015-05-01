# cs51-final-project



To compile our program, just run "make" in the code directory from terminal.

You can also run "make clean" if there are any issues during compilation.




To run the program, run ./Heap.native [csv] [cutoff].

The program takes 2 command line arguments.

The first is the filepath of the csv you wish to use for the data. All of the csv files in our data subdirectory are in the correct format (string float float, separated by semicolons). usdata.csv will have the most cities to choose from!

The second command line argument is the cutoff distance--aka, what is the acceptable distance between two cities for your trip. This value should be in kilometers, and should be numerical (either an int or a float is fine; the program will convert either to a float!).