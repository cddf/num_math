#!/usr/bin/env Rscript
args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
  print(paste(args[1]," is not a readable file!"))
  quit();
}
input = read.table(args[1], header=T)

if(length(unique(input$x)) != length(input$x))
{
  print("The input file has contradicting points")
  quit()
}
#print(input)

# (i)
calc = function(x, y, n = 1)
{
  if (length(y) == 1)
  {
    return (c(y[1]))
  }
  else
  {
    Y = c()
    for(i in 1:(length(y)-1))
    {
      newValue = (y[i+1] - y[i]) / (x[i+n] - x[i])
      Y = append(Y,newValue)
    }
    
    result = append(calc(x,Y,n+1), y[1], after=0)
    return (result)
  }

}

print (calc(input$x, input$y))

# (ii)
wert = function(x, X, Y)
{
  koeff = calc(X, Y)
  X = x - X
  X = cumprod(X)
  result = 0
  for(i in 1:length(koeff))
  {
    if(i==1)
      result = result + koeff[i]
    else
      result = result + koeff[i] * X[i-1]
  }
  return(result)
}
print (wert(1,input$x,input$y))
