#!/usr/bin/env Rscript
f = function(x)
{
  return (exp(-x)-1e-9)
}
df = function(x)
{
  return (-exp(-x))
}

x = 0
epsilon = 1e-100

xl = x + 2*epsilon
i = 0
while(abs(xl-x) > epsilon)
{
  xl = x
  x = x - f(x)/df(x)
  print(x)
  i = i+1
}
print(i)
