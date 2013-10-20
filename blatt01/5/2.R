#!/usr/bin/env Rscript

# args: x input-file
# out:  result-file, plot

args = commandArgs(TRUE)

x = as.numeric(args[1])
if(file.access(args[2]) == -1 || file.access(args[2],mode = 4) == -1)
{
  print(paste(args[2]," is not a readable file!"))
  quit();
}
input = read.table(args[2], header=T)

# function to plot
f = function(x)
{
  return( (x-1)^2 )
}

# (ii)
wert = function(x, xi, cn)
{
  X = x - xi
  X = cumprod(X)
  result = 0
  for(i in 1:length(cn))
  {
    if(i==1)
      result = result + cn[i]
    else
      result = result + cn[i] * X[i-1]
  }
  return(result)
}
pn = wert(x,input$xi,input$cn)
write.table (pn, file="2.out", row.names = F, col.names = F)

y = c()
fi = c()
for(i in -10:10)
{
  y = append(y, wert(i,input$xi,input$cn))
  fi = append(fi,f(i))
}
  
plot(-10:10, y, type="l")
points(x,pn)
points(x,f(x))
lines(-10:10, fi)
