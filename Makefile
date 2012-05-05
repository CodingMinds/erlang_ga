# target: all   	- Default target. Compiles .erl files
all:
	erlc environment.erl individual.erl monitor.erl monitor_trigger.erl

# target: clean 	- Remove compilation results
clean:
	rm 2> /dev/null *.beam

# target: plot  	- Plot default_results with gnuplot
plot:
	@gnuplot simple.plot

# target: help  	- Display callable targets
help:
	@egrep "^# target:" [Mm]akefile
