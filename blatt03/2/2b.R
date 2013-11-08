#!/usr/bin/env Rscript

x = 0:10
x = 1/(2^x)

y=x^2*sin(x)/8

z=sin(x/2)-sin(x)*(x/2)/x


plot(log(x),log(y), col="red")
points(log(x),log(z), col="green")
