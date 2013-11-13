#!/usr/bin/env Rscript

# args: input-file x

args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
  print(paste(args[1]," is not a readable file!"))
  quit();
}
input = read.table(args[1], header=T)
x = as.double(args[2])
xi = input$xi
Mi = input$Mi
yi = input$yi

# calc Bi, Ai
Bi = c()
Ai = c()
for(i in 2:length(xi))
{
  h = xi[i]-xi[i-1]
  Bi = append(Bi, yi[i-1] - Mi[i-1] * h^2 / 6)
  Ai = append(Ai, (yi[i] - yi[i-1]) / h - (Mi[i] - Mi[i-1]) * h / 6)
}

# function to plot
f = function(x)
{
  #return( exp(x) )
  #return( 1/(1+25*x^2) )
  return(x)
}

Si = function(xi, Mi, Ai, Bi, x)
{
  i = 2
  repeat
  {
    if(i > length(xi))
    {
      print("x ist zu außerhalb des Splines!")
      print(x)
      quit()
    }
    else if(x >= xi[i-1] && x <= xi[i])
    {
      break
    }
    i = i+1
  }
  tmp =       Mi[i] * (x-xi[i-1])^3 / (6 * (xi[i]-xi[i-1]))
  tmp = tmp + Mi[i-1] * (xi[i]-x)^3 / (6 * (xi[i]-xi[i-1]))
  S = tmp + Ai[i-1] * (x-xi[i-1]) + Bi[i-1]

  return(S)
}

Sx = Si(xi,Mi,Ai,Bi,x)
print(Sx)

y = c()
fi = c()
min = xi[1]
max = xi[length(xi)]
for(i in (100*min):(100*max))
{
  X = i/100.0
  y = append(y, Si(xi,Mi,Ai,Bi,X))
  fi = append(fi,f(X))
}
plot(((100*min):(100*max))/100, y, type="l", xlab='x', col = "red")
points(x,Sx)
lines(((100*min):(100*max))/100, fi, col = "blue")
