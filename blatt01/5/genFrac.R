#!/usr/bin/env Rscript


# Arg 1 is n Arg 2 is the output fileName, ".dat" added
args <- commandArgs(TRUE)

#args[1]
#args[2]
#as.integer(args[5]) 

if(suppressWarnings(is.na(as.integer(args[1]))))
{
  print(paste("First argument", args[1], "has to be an integer"))
  quit();
}

if(is.na(args[2]))
{
  print(paste("An output file name is needed!"))
  quit()
}
n = as.integer(args[1])

h = 2/n
x = 0:n
x = -1 + x*h

y = 1 / (1 + 25 * x^2)

write.table(cbind(x,y), file=paste(args[2],".dat", sep=""), row.names = FALSE) 

