#!/bin/bash
./genExp.R $1 exp && ./1.R exp.dat && ./2.R $2 1.out
