all:
	erlc environment.erl individual.erl monitor.erl

clean:
	rm 2> /dev/null *.beam

plot:
	gnuplot simple.plot
