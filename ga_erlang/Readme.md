Genetic Algorithm (Erlang)
==========================

## Description
A simple genetic algorithm where each genome / individual is a single process.

Hint: Implementation without crossover !

## Structure

# individual.erl
The gen_server which represents the indivdiual
# environment.erl
The gen_server which holds the population and manages the individuals
# monitor.erl
A simple monitor which writes some statistics to a plain text file
This file can be plot with ```sh
gnuplot simple.plot```

## Licence
[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
