#!/usr/bin/env Rscript

x = c(0,1,1,2,4,5);
y = c(0,-1,1,1,3,2);

S_k <- function(x,k)
{
  return (sum(x^k))
}

S_kl <- function(x,k,l)
{
  return(S_k(x,k)*S_k(x,l))
}

result = x^2-(S_k(x,4)/S_k(x,2));
result = result - (S_kl(x,2,3)-S_kl(x,1,4))/ (S_kl(x,2,2)-S_kl(x,1,3))*(x-S_k(x,3)/S_k(x,2));
result = sum(result * y);


coeff0 = (S_kl(x,2,2)-S_kl(x,1,3))*(S_kl(x,2,2)-S_kl(x,0,4));
coeff0 = coeff0 - (S_kl(x,2,3)-S_kl(x,1,4))*(S_kl(x,1,2)-S_kl(x,0,3));
coeff0 = coeff0 / S_k(x,2) / (S_kl(x,2,2)-S_kl(x,1,3));

a0 = result/coeff0;
print(sprintf("a0: %f", a0));

result = x - S_k(x,3)/S_k(x,2);
result = sum(result*y);

coeff0 = (S_kl(x,1,2)-S_kl(x,0,3)) / S_k(x,2);
coeff1 = (S_kl(x,2,2)-S_kl(x,1,3)) / S_k(x,2);

a1 = (result-coeff0*a0)/coeff1;
print(sprintf("a1: %f", a1));

result = sum(y)

coeff0 = S_k(x,0);
coeff1 = S_k(x,1);
coeff2 = S_k(x,2);

a2 = (result - coeff0*a0 - coeff1*a1) / coeff2;
print(sprintf("a2: %f", a2));
