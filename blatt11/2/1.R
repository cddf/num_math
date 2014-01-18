#!/usr/bin/env Rscript

print("a");

x0 = 0.14;
print(sprintf("Startwert = %f", x0));

a=function(x)
{
  return (x^2+x-log(x)-2);
}
b=function(x)
{
  return (sqrt(log(x)+2));
}
c=function(x)
{
  return (exp(x^2-2))
}

loop=function(x0,func,N)
{
  print(func);
  print(sprintf("Startwert = %f", x0));
  x=x0;
  for(i in 1:N){x=func(x); print(sprintf("%d: %f",i, x));}
}

loop(x0,a,10);
loop(x0,b,10);
loop(x0,c,10);
