set title 'GA Fitness'
#set xrange [95000:100000]
set yrange [50:100]
set xlabel 'Iterations'
set ylabel 'Population fitness [%]'

# max possible avg value: 65025.0
plot 'default_results' using 1:($3/650.25) with steps title ""

set output "default_results.png"
set terminal png
replot
