#!/usr/bin/env Rscript



f=function(x)
{
  return (x-atan(x));
}

fNull2=function(x)
{
  return (x-(x-atan(x))/(x^2/(x^2+1)));
}

fNull=function(x)
{
  return (x-(x^2+1)*(x-atan(x))/(x^2));
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

x0 = 10000000;
loop(x0, f, fNull,40);
