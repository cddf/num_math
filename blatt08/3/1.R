#!/usr/bin/env Rscript
C = matrix(NA,6,3);
 C[,1] = c(1,1,1,1,1,1)
 C[,2] = c(0,sqrt(3)/2,sqrt(3)/2,1,-sqrt(3)/2,-sqrt(3)/2)
 C[,3] = c(1,1/2,-1/2,0,-1/2,1/2)
svd(C)
C_svd = svd(C)
d=c(1.9,3,13/5,11/10,2/5,3/2)
x= C_svd$v %*% ((t(C_svd$u) %*% d) / C_svd$d)
x
