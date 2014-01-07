#!/usr/bin/env Rscript

A = matrix(NA, 3, 3);
A[,1] = c(2, -1, 0);
A[,2] = c(-1, 2, -1);
A[,3] = c(0, -1, 1);
b = c(1, 0, 1);

x = c(1, 0, 1);
print(x);

r = b - A %*% x;
#print(r);
d = r;
repeat
{
  rProd = (t(r) %*% r)[1,1];
  #print(rProd);
  z = A %*% d;
  #print(z);
  a = rProd/(t(d) %*% z)[1,1];
  #print(a);
  x = x + (a * d);
  r = r - (a * z);
  beta = (t(r) %*% r)[1,1]/rProd;
  d = r + beta * d;
  print(x);
  if(sqrt((t(r)%*%r)[1,1]) < 0.001)
  {
    break;
  }
}
