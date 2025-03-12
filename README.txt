User Instructions + Overview: 

Users can input into a Rackette string representing a raw program, for instance “(+ 3 2)”. This will first be read into a concrete program,
then parsed into an abstract program, processed to a list of values, and finally string of list will apply to the elements of the list of values
to produce a string representing the output to the user. 

The user would expect from this string the output “[5]”. 


Possible Bugs of Program:

The program can take in and correctly evaluate recursive procedures such as easyCondEasyGo (a simple 1+2+3 … + n sum) involving numbers. 
However, the program does not work properly for many list-recursive outputs. We were unable to get the program to work for ksubsetsum, unfortunately.




