#!/usr/bin/env Rscript

# args: input-file boundary_conditions boundary_conditions_args
# out:  result-file

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
n = length(input$x) - 2
x = input$x
y = input$y

if(suppressWarnings(is.na(as.integer(args[2]))))
{
  print("The second argument need to be an integer")
  quit()
}

df = function(a, i)
{
  return (a[i]-a[i-1])
}

lambda = vector()
my = vector()
d = vector()

x
for(i in 2:(n+1))
{
  lambda[i] = df(x,(i+1)) / (df(x, (i+1)) + df(x, i))
  my[i] = 1 - lambda[i]
  d[i] = 6 / (df(x, (i+1)) + df(x, i))*(df(y,(i+1))/df(x,(i+1)) - df(y, i)/df(x, i))
}
lambda
M = c()
 
if(args[2] == 2)
{
  if(y[1] != y[n+2])
  {
    print("first and last value needs to have same y value")
    quit()
  }
  lambda[n+2] = df(x,2)/(df(y,(n+2))+df(y, 2))
  my[n+2] = 1 - lambda[n+2]
  d[n+2] = 6/(df(y,(n+2))+df(y,2))*(df(y,2)/df(x,2)-df(y,(n+2))/df(x,(n+2)))


  for(i in 1:(n+1))
  {
    if(i > 2)
    {
      M = append(M,rep(0,(i-2)))
    }
    if(i > 1)
    {
      M = append(M,my[i])
    }
    M = append(M,2)
    if(i<(n+1))
    {
      M = append(M,lambda[i+1])
    }
    if(i<(n+1))
    {
      M = append(M,rep(0,(n-i)))
    }
  }
   
  M = matrix(M, ncol=(n+1), byrow=TRUE)
  d = d[2:(n+2)]
} else {
  if(args[2] == 3)
  {
    if(suppressWarnings(is.na(as.double(args[3])))
       || suppressWarnings(is.na(as.double(args[4]))))
    {
      print(paste("Third and fourth argument has to be doubles"))
      quit();
    }
    lambda[1] = 1
    d[1] = 6/df(x,2)*(df(y,2)/df(x,2)-as.double(args[3]))
    my[n+2] = 1
    d[n+2] = 6/df(x,(n+2))*(as.double(args[4])-df(y,(n+2))/df(x,(n+2)))
  } else {
    lambda[1] = 1
    d[1] = 0
    my[n+2] = 0
    d[n+2] = 0
  }
  for(i in 1:(n+2))
  {
    if(i > 2)
    {
      M = append(M,rep(0,(i-2)))
    }
    if(i > 1)
    {
      M = append(M,my[i])
    }
    M = append(M,2)
    if(i<(n+2))
    {
      M = append(M,lambda[i])
    }
    if(i<(n+1))
    {
      M = append(M,rep(0,(n+1-i)))
    }
  }
  M = matrix(M, ncol=(n+2), byrow=TRUE)
}

M
d
moments = solve(M, d)
A = vector()
B = vector()
for(i in 1:length(moments))
{
  print(sprintf("moment[%d] = %f", (i-1), moments[i]))
  if(i > 1)
  {
    A[i] = df(y,i) / df(x, i) - df(x, i) / 6 * (moments[i] - moments[i-1])
    B[i] = y[i-1] - moments[i-1]*(df(x,i))^2/6

    print(sprintf("A[%d] = %f", (i-1), A[i]))
    print(sprintf("B[%d] = %f", (i-1), B[i]))
  }
}

#print(input)


output = paste(sub("(.+)[.][^.]+$", "\\1", basename(args[1])), ".out",sep="")

result = data.frame("xi"=input$x, "Mi"=moments)
write.table (result, file=output, row.names = FALSE, col.names = T)

