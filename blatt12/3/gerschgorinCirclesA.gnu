
set terminal postscript
set output '| ps2pdf - gerschgorinCirclesA.pdf'

set parametric;
plot 39*sin(t)+20,39*cos(t) lt 1,18*sin(t)+1,18*cos(t) lt 1,20*sin(t)-19,20*cos(t) lt 1, 19*sin(t)+20,19*cos(t) lt 2,20*sin(t)+1,20*cos(t) lt 2,38*sin(t)-19,38*cos(t) lt 2;
