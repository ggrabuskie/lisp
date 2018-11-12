CPSC 481-03
Project 2: GP Arithmetic Exprs
Team: GBA
Team Members
Georden Grabuskie	ggrabuskie@csu.fullerton.edu
Alessandro Quezada	sandroq95@csu.fullerton.edu

Introduction
The purpose of this program is the implement Genetic Programming. A population of random expressions is created. These expressions
are then executed and given a fitness score. The expressions are the mutated and crossed over to so that the next generation of 
expressions are more fit than the last.

Files List
- project2.lisp
- README.txt

Installation/Run
- compile in a common lisp environment
- the "main" function is what runs the program. Run (main 50 50) to run for 50 generations of size 50.

Missing Features
We were not able to implement the crossover function. Because of this, the population does not increase in fitness as much as it should.
Instead, we rollover the best fit expressions and mutate them, which produces little change in fitness scores. 
