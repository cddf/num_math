#!/usr/bin/env Rscript

# args: <input-file with integral>

eval(parse(file = "simplifications.R"));
args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
  print(paste(args[1]," is not a readable file!"))
  quit();
}

eval(parse(file = args[1]));
D(f, "x");
if(FALSE)
{
# Calculate Bernoulli numbers
B = vector();
A = vector();
for(m in 0:10) {
  A[m+1] = 1/(m+1);
  if(m >= 1) {
    for(j in seq(m,1)) {
     A[j] = j * (A[j] - A[j+1])
    }
  }
  B[m+1] = A[1] 
 }
B
}
if(FALSE) {
derivations = vector()
derivations[1] = as.expression(D(f, "x"));


#derivations[2] = D(derivations[1],"x");
for(i in 2:(2*m+2))
{
  derivations[i] = as.expression(D(derivations[i-1], "x"))
}

derivations


t = vector()
derivations[1]
substitute(derivations[1],list(x=b))

#gsub('x',a,derivations[1])
for(i in 1:m)
{
  derivationA = eval(parse(text=gsub('e\\^', 'exp',gsub('x',a,derivations[2*i-1]))));
  derivationB = eval(parse(text=gsub('e\\^', 'exp',gsub('x',b,derivations[2*i-1]))));
  t[i+1] = B[2*i+1]/factorial(2*i) * (derivationB-derivationA);
}
}

T=vector();
n = 2^(seq(0,m-1));
n
#Trapezregel
for(i in 1:m)
{
 h = (b - a) / n[i];

 T[i] = 0
 for(j in (0:n[i]))
 {
  x = a + j * h;
  value = eval(parse(text=gsub('e\\^', 'exp',gsub('x',x,f))));
  if(0 < j && j < n[i]) {
    #print("A oder B");
    T[i] = T[i] + value;
  } else {
    #print("M");
    T[i] = T[i] + 0.5 * value;
  }

 }

  T[i] = c * h * T[i];
 #print("M-Wert")
 #print(T[i])
}
T[1]
T[m+1] = T[1]
print("T[j]")
for(i in 1:(m-1))
{
  for(j in 1:(m-i))
  {
    #print(T[j+1])
    T[j] = T[j+1] + (T[j+1]-T[j]) / ((n[j+i]/n[j])^2 -1)
  }
  T[m+1-i] = T[1]
  print(T[1])
}
print("D")
plot((b-a)/n,rev(T[2:(m+1)]));
plot(n, rev(T[2:(m+1)]), log =  "x");
#result = data.frame("xi"=input$x, "Mi"=moments, "yi"=input$y)
#write.table (result, file=output, row.names = FALSE, col.names = T)

