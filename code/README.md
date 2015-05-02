# cs51-final-project

To compile our program, just run "make" in the code directory from terminal.

You can also run "make clean" if there are any issues during compilation.

To run the program, run ./Heap.native [csv] [cutoff].

[csv] refers to the filepath to the csv file that will act as the data the program 
uses. There are a three csv files inside of data/ (we’ll talk about them later)

[cutoff] is a float that refers to the cutoff distance, i.e., the maximum distance 
(in km) that you want to travel in one trip/day. When the distance between cities 
smaller than the cutoff, an edge is created between them; otherwise they exist 
separate from each other in the graph. [cutoff] can be passed in as either an int
or a float.

Once the program is run, the program will prompt you for the Origin City 
and Destination City.
Make sure that you fill it in as cityname comma stateabbreviation. 
For example: New York City, NY is a valid input. 

The csv files are as follows: 

The first one, toydata, is a file containing only 8 cities. It is used to ensure 
that the program works. When cutoff is passed in as 300.0 (or 300), and "Boston, MA" 
and "Cleveland, OH" are passed in as the start and end points respectively, the program 
should have the following output:

Boston, MA -> Albany, NY -> Syracuse, NY -> Buffalo, NY -> Cleveland, OH
Total Distance: 957.73 km 

The second csv file, smalldata.csv, is a csv file containing the 8681 biggest cities/towns
in America. Pass the file in if you want to work with a more realistic dataset and to have
a pretty solid range of cities/towns to choose from. 

The third csv file, usdata.csv, contains 30617 of America’s cities/towns. This is a dataset 
to really push the program to its limits. 
