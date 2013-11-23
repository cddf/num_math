#!/usr/bin/env Rscript

# args: <input-file with integral>

args = commandArgs(TRUE)

if(file.access(args[1]) == -1 || file.access(args[1],mode = 4) == -1)
{
  print(paste(args[1]," is not a readable file!"))
  quit();
}

eval(parse(file = args[1]));

rombergSequence = function(m) {
  return (2^(seq(0,m-1)))
}

bulirschSequence = function(m) {
  seqHalflength = m/2;
  sequence = rep(2^seq(1,seqHalflength), each=2);
  sequence = sequence * rep(c(1,1.5), out.length=(2*seqHalflength));
  sequence = append(sequence[1:(m-1)], 1, after=0);
  return (sequence);
}

sumTrapez = function(f,a,b,n) {
T=vector();
#Trapezregel
m = length(n);
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
return (T);
}


sumSimpson = function(f,a,b,n) {
S=vector();
#Simpsonregel
m = length(n);
for(i in 1:m)
{
 h = (b - a) / n[i];

 S[i] = 0
 for(j in (0:n[i]))
 {
  x = a + j * h;
  value = eval(parse(text=gsub('e\\^', 'exp',gsub('x',x,f))));
  xMiddle = x + 0.5*h
  if(j < n[i])
  {
    valueMiddle = eval(parse(text=gsub('e\\^', 'exp',gsub('x',xMiddle,f))));
    S[i] = S[i] + 4 * valueMiddle;
  }
  if(0 < j && j < n[i]) {
    S[i] = S[i] + 2 * value;
  } else {
    S[i] = S[i] + value;
  }

 }

  S[i] = c * h / 6 * S[i];
}
return (S);
}

rombergIntegration = function(f,a,b,n) {
T=sumTrapez(f,a,b,n);

plot(n,T, log = "x", main = paste(main," -  trapezium rule"));

S=sumSimpson(f,a,b,n);

plot(n,S, log = "x", main = paste(main," -  Simpson's rule"));


T[m+1] = T[1]
#print("T[j]")
for(i in 1:(m-1))
{
  for(j in 1:(m-i))
  {
    #print(T[j+1])
    T[j] = T[j+1] + (T[j+1]-T[j]) / ((n[j+i]/n[j])^2 -1)
  }
  T[m+1-i] = T[1]
  #print(T[1])
}
T = rev(T[2:(m+1)]);
#plot((b-a)/n,T, main = main);
plot(1:length(T), T, xlab = "i", main = paste(main, " -  Values by index"));


plot(1:length(T), abs(exactResult-T), log="y", xlab = "i", main = paste(main, " -  Errors by index"));
}

main = "Romberg sequence";
rombergIntegration(f,a,b,rombergSequence(m))
main = "Bulirsch sequence";
rombergIntegration(f,a,b,bulirschSequence(m))
#result = data.frame("xi"=input$x, "Mi"=moments, "yi"=input$y)
#write.table (result, file=output, row.names = FALSE, col.names = T)

