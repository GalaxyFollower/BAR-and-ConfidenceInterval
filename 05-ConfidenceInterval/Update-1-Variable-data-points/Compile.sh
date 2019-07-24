#! /bin/bash -x
rm *.o *.mod *.exe
gfortran -c ConfidenceProcedures.f90
gfortran -o ConfidenceCalculator-Update-1.exe ConfidenceMain.f90 ConfidenceProcedures.o 
rm *.o *.mod
