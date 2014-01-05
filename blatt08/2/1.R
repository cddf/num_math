#!/usr/bin/env Rscript
C = matrix(NA,6,3);
C[,1] = c(1,1,1,1,1,1)
C[,2] = c(0,sqrt(3)/2,sqrt(3)/2,1,-sqrt(3)/2,-sqrt(3)/2)
C[,3] = c(1,1/2,-1/2,0,-1/2,1/2)
Cqr = qr(C)
R = qr.R(Cqr)
Q = qr.Q(Cqr)
d=c(1.9,3,13/5,11/10,2/5,3/2)
d_Q = t(Q)%*%d
x = backsolve(R,d_Q);
x
