#!/usr/bin/env Rscript

A = matrix(c(2,0,0,10),nrow=2)
b = matrix(c(0,0),nrow=2)
x = matrix(c(-50,10),nrow=2)

N = 100
X = c()
for(n in 1:N)
{
  d = b - A %*% x
  #print(d)
  a = as.double((t(d) %*% d) / (t(d) %*% A %*% d))
  #print(a)
  x = x + a * d
  X = append(X,as.double(t(x) %*% x))
}

plot(1:N,X, log="y")
