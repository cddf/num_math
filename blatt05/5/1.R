#!/usr/bin/env Rscript

# args: <input-file with integral>

args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
  print(paste(args[1]," is not a readable file!"))
  quit();
}

eval(parse(file = args[1]));
D(f, "x");

# Calculate Bernoulli numbers
B = vector();
n=10
A = vector();
for(m in 0:n) {
  A[m+1] = 1/(m+1);
  if(m >= 1) {
    for(j in seq(m,1)) {
     A[j] = j * (A[j] - A[j+1])
    }
  }
  B[m+1] = A[1] 
 }
B
Derivatives = vector()
Derivatives[1] = as.expression(D(f, "x"));

#Derivatives[2] = D(Derivatives[1],"x");
for(i in 2:(2*m+2))
{
  Derivatives[i] = as.expression(D(Derivatives[i-1], "x"))
}

#result = data.frame("xi"=input$x, "Mi"=moments, "yi"=input$y)
#write.table (result, file=output, row.names = FALSE, col.names = T)

