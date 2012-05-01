set title 'GA Fitness'
set yrange [80:100]
set xlabel 'Iteration (* 1000)'
set ylabel 'Population fitness [%]'

plot 'default_results' using 1:($2/2.52) with steps title ""

set output "datei.png"
set terminal png
replot
