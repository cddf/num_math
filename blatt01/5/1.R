#!/usr/bin/env Rscript
input = read.table("points.dat", header=T)
print(input)

# (i)
calc = function(x, y, n = 1)
{
  if (length(y) == 1)
  {
    return (y[1])
  }
  else
  {
    Y = c()
    for(i in 1:(length(y)-n+1)) # TODO HÄÄÄÄ???
    {
      Y = c(Y, (y[i+1] - y[i]) / (x[i+n] - x[i]))
    }
    result = c(y[1], calc(x,Y,n+1))
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
print (wert(4,input$x,input$y))
