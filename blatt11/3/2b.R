#!/usr/bin/env Rscript



f=function(x)
{
  return (x-atan(x));
}

fNull=function(x)
{
  return (x-atan(x)*(x^2+1));
}

fNull2=function(x)
{
  lambda = 1.0;
  delta = - atan(x) * (x^2 + 1);
  repeat
  {
    if(abs(x+lambda*delta) < abs(x) || lambda < 1e-4) 
    {
      break;
    }
    lambda = 0.5 * lambda;     
  }
  if(lambda < 1e-4)
  {
    return (x);
  }
  else
  {
    return (x+lambda*delta)
  }
}

loop=function(x0,func,funcNull)
{
  print(sprintf("Startwert = %f", x0));
  x=x0;
  i = 0;
  repeat
  {
    xold = x;
    x=funcNull(x);
    if(x == xold)
    {
      break;
    }
    error1 = abs(func(x));
    error2 = abs(x-xold); 
    print(sprintf("%d: %f, Error1: %e, Error2: %e",i, x, error1, error2));
    if(error1 < 1e-6)
    {
      break;
    }
    ++i;
  }
}

x0 = 1.4;
loop(x0, f, fNull2);
