# king-snake-cube
## A Haskell demo

The king snake cube is a wooden puzzle comprising 64 small wooden cubes joined on an elastic thread.
Some cubes are threaded 'straight through' and some 'right-angled'.
The result is that the cubes can be flattened out to look like a snake, or can be convolved into various shapes in three dimensions.

The challenge (which, without the aid of a computer, is considerable) is to wind the snake into a 4x4x4 cube.

Taking a brute-force approach we can use recursion to find a solution.
The main steps are:

* Represent the geometry of the snake (the sequence of cubes and whether they are threaded straight through or right-angled)
* Represent a partial solution as a list of (valid) cube positions
* Calculate the set of possible positions for the next cube
* Work out if a possible position is valid (within the bounds of a 4x4x4 cube, and not a position which is already occupied)
* Choose a starting point
* Use recursion to solve the cube 
