Genetic Algorithm (Erlang)
==========================

## Description
A simple genetic algorithm where each genome / individual is a single process.

Hint: Implementation without crossover !

## Structure

#### individual.erl
The gen_server which represents the indivdiual
#### environment.erl
The gen_server which holds the population and manages the individuals
#### monitor.erl
A simple monitor which writes some statistics to a plain text file
This file can be plot with
```sh
gnuplot simple.plot
```

## Usage

Before the first run (or after sourcecode modifications) you need to compile the .erl files
```sh
erlc environment.erl individual.erl monitor.erl
```

To start the simple default environment use
```sh
erl -s monitor start -s environment start
```

and initialize evolution with
```erlang
environment:tick(Count).
```
where Count is the amount of iterations which should be calculated.

Results will be automatically saved in __default_results__.

The actual state could be requested with
```erlang
environment:gene_pool().
```
or
```erlang
environment:fitness().
```
.

For further details check the source code annotations.

## Performance

Intel(R) Core(TM) i7 CPU 950 @ 3.07GHz (mostly idle):

100,000 cycles with default parameters are computed in round about 4 minutes.

## Licence
[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
