#!/usr/bin/env Rscript

# args: x input-file
# out:  result-file

args = commandArgs(TRUE)

x = as.numeric(args[1])
if(file.access(args[2]) == -1 || file.access(args[2],mode = 4) == -1)
{
  print(paste(args[2]," is not a readable file!"))
  quit();
}
input = read.table(args[2], header=T)

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
write.table (wert(x,input$xi,input$cn), file="2.out", row.names = F, col.names = F)
