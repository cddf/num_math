#!/usr/bin/env Rscript

x0 = 10;

f=function(x)
{
  return (exp(-x)-10^(-9));
}

fNull=function(x)
{
  return (x+(1-10^(-9)/exp(-x)));
}

loop=function(x0,func,funcNull,N)
{
  print(sprintf("Startwert = %f", x0));
  x=x0;
  for(i in 1:N)
  {
    xold = x;
    x=funcNull(x);
    error1 = abs(func(x));
    error2 = abs(x-xold); 
    print(sprintf("%d: %f, Error1: %e, Error2: %e",i, x, error1, error2));
  }
}

loop(x0, f, fNull,100);
