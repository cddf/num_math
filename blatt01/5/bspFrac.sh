#!/bin/bash
./genFrac.R $1 frac && ./1.R frac.dat && ./2.R $2 1.out
