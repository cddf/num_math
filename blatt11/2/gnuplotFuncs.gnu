set terminal postscript
set output '| ps2pdf - a.pdf'
set xrange[-0.2:3]
set yrange[-1:6]
plot x**2+x-log(x)-2, x
reset
set terminal postscript
set output '| ps2pdf - b.pdf'
set xrange[-0.2:3]
set yrange[0.2:3]
plot sqrt(log(x)+2), x
reset
set terminal postscript
set output '| ps2pdf - c.pdf'
set xrange[-1.5:1.8]
plot exp(x**2-2), x
