#!/usr/bin/env Rscript

# args: input-file polynomgrad
# out:  result-file

args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
    print(paste(args[1]," is not a readable file!"))
  quit();
}
input = read.table(args[1], header=T)

if(suppressWarnings(is.na(as.integer(args[2]))))
{
  print("The second argument need to be an integer")
  quit()
}


n = as.integer(args[2]);
if((n+1) > length(unique(input$x))) {
  print("It's not allowed, that n is bigger than the count of different x values.")
  quit()
}
nPone = n + 1;
#if(length(unique(input$x)) != length(input$x))
#{
#  print("The input file has contradicting points")
#  quit()
#}
#print(input)

x = input$x;
y = input$y;

S_k = function(x,k)
{
    return (sum(x^k))
}

S_y_k = function(x,y,k)
{
    return (sum(y*(x^k)))
}



C <- matrix(data=NA, nrow=nPone, ncol=nPone);
d <- rep(NA, nPone);

for(i in 0:n)
{
  C[(i+1),(i+1)] = S_k(x,i+i)
  d[(i+1)] = S_y_k(x, y, i)
  if(i < n) {
    for(j in (i+1):n)
    {
      C[(i+1),(j+1)] = S_k(x,i+j)
      C[(j+1),(i+1)] = C[(i+1),(j+1)]
    }
  }
}


L_C <- chol(C);

s<-forwardsolve(t(L_C), d);
t<-backsolve(L_C,s);

t

t2 = try(qr.solve(C,d));

t2

polynomSingle = function(x,a=t)
{
  return (sum(a*x^(0:(length(t)-1))));
}
#curve(x^2);
polynom = Vectorize(polynomSingle, "x")
#a = expression(x^3);



plot(polynom, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)));

points(x, y);

#result = data.frame("xi"=input$x, "cn"=calc(input$x, input$y))
#write.table (result, file="1.out", row.names = FALSE, col.names = T)

