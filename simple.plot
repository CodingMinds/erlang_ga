set title 'GA Fitness'
#set xrange [95000:100000]
set yrange [40:100]
set xlabel 'Iterations'
set ylabel 'Population fitness [%]'

# max possible value: 650250
plot 'default_results' using 1:($2/6502.50) with steps title ""

set output "default_results.png"
set terminal png
replot
