#!/usr/bin/env Rscript

# args: <input-file with integral> <error tolerance (3 for 10e-3)>

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

Trapez = function(f,a,b,n) {
  T=vector();
  #Trapezregel
  h = (b - a) / n;

  for(j in (0:n))
  {
    T[j] = 0
    x = a + j * h;
    value = eval(parse(text=gsub('e\\^', 'exp',gsub('x',x,f))));
    if(0 < j && j < n) {
      #print("A oder B");
      T[j] = T[j] + value;
    } else {
      #print("M");
      T[j] = T[j] + 0.5 * value;
    }

  }
  T = c * h * T;
  return (T);
}

Simpson = function(f,a,b,n) {
  S=vector();
  #Simpsonregel
  h = (b - a) / n;

  for(j in (0:n))
  {
    S[j] = 0
    x = a + j * h;
    value = eval(parse(text=gsub('e\\^', 'exp',gsub('x',x,f))));
    xMiddle = x + 0.5*h
    if(j < n)
    {
      valueMiddle = eval(parse(text=gsub('e\\^', 'exp',gsub('x',xMiddle,f))));
      S[j] = S[j] + 4 * valueMiddle;
    }
    if(0 < j && j < n) {
      S[j] = S[j] + 2 * value;
    } else {
      S[j] = S[j] + value;
    }

  }

  S = c * h / 6 * S ;

  return (S);
}

adaptiveIntegration = function(f,a,b,n,epsilon) {
  # Solve
  T=Trapez(f,a,b,n);
  counter <<- counter +1
  S=Simpson(f,a,b,n);

  # Estimate
  err = abs(T-S)
  #print(err)

  # Mark & Refine
  h = (b - a) / n;
  for(i in 1:n)
  {
    if(err[i] >= epsilon)
    {
      a2 = a + h * (i-1)
      b2 = a + h * (i)
      if(a2 != b2 && (b2 != b || a2 != a))
      {
        T[i] = adaptiveIntegration(f,a2,b2,2,epsilon)
      }
      else
      {
        print(a)
        print(b)
      }
    }
  }
  return(sum(T))
}

main = "Romberg sequence";
rombergIntegration(f,a,b,rombergSequence(m))
main = "Bulirsch sequence";
rombergIntegration(f,a,b,bulirschSequence(m))

print("exact Result:")
print(exactResult)
err = args[2]
count = vector()
for(x in 1:err)
{
  counter = 0
  print("adaptive Integration")
  int = adaptiveIntegration(f,a,b,2,1/10^x)
  print(int)
  print("Anzahl Funktionsaufrufe:")
  count[x] = counter
  print(counter)
}
#plot(1:err,int)
err = (1:err)
plot(err,count, main="Number of function calls by error 1e-x")

#result = data.frame("xi"=input$x, "Mi"=moments, "yi"=input$y)
#write.table (result, file=output, row.names = FALSE, col.names = T)

