set terminal postscript
set output '| ps2pdf - a.pdf'
plot (x+16/(2*x)-x/2), x
set xrange[0.1:40]
set yrange[0:20]
plot (x+16/(2*x)-x/2), x
reset
set xrange[-40:0.1]
set yrange[-20:0]
plot (x+16/(2*x)-x/2), x
