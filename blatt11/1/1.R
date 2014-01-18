#!/usr/bin/env Rscript


G=function(x)
{
  return (x-cos(x))
}

GNull=function(x)
{
  return (x-(x-cos(x))/(1+sin(x)))
}


G2Null=function(x)
{
  return (x-(1+sin(x)-sqrt(2*(1+sin(x)-x*cos(x))+(cos(x))^2))/cos(x))                                            
}


loop=function(x0,func,funcNull,N)
{
  print(funcNull);
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

x0 = pi/8;
loop(x0, G, GNull,5);

x0 = pi/8
loop(x0, G, G2Null,5);

x0 = pi;
loop(x0, G, G2Null,5);
