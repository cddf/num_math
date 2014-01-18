#!/usr/bin/env Rscript

f=function(x)
{
  return (x^2-a);
}

fNull=function(x)
{
  return (x+0.5*(a/x-x));
}

f2Null=function(x)
{
  return (x*(x^2+3*a)/(3*x^2+a));
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

a = 16;
x0 = 2.5;
loop(x0, f, fNull,10);


loop(x0, f, f2Null,10);
