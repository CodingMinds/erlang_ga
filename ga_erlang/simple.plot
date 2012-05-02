set title 'GA Fitness'
set yrange [80:100]
set xlabel 'Iteration (* 10)'
set ylabel 'Population fitness [%]'

plot 'default_results' using 1:($2/10.08) with steps title ""

set output "datei.png"
set terminal png
replot
