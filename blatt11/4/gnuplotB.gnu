set terminal postscript
set output '| ps2pdf - b.pdf'
plot (x*(x**2+3*16)/(3*x**2+16)), x
set xrange[-2:7]
set yrange[-2:7]
plot (x*(x**2+3*16)/(3*x**2+16)), x
reset
set xrange[-7:2]
set yrange[-7:2]
plot (x*(x**2+3*16)/(3*x**2+16)), x
