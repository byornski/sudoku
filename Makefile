#FC = /cygdrive/c/Program\ Files\ \(x86\)/Intel/Compiler/11.0/061/fortran/bin/intel64/ifort.exe
#optimised
#flags = -O3 
#debug
#flags = /traceback /Og /check:all
#ldflags = *.obj

FC = gfortran
ldflags = *.o
flags = -g -pg -fbounds-check -Wall -fbacktrace -finit-real=nan -ftrapv -fdefault-integer-8 -Og -fimplicit-none -Wall
#flags = -O2 -pg -g
#flags = -O3 -pg -g -fdefault-integer-8
#flags = -O3  -fdefault-integer-8
all: utils stack
	$(FC) sudoku.f90 $(ldflags) $(flags)
utils: stack
	$(FC) -c utils.f90 $(flags)
stack: 
	$(FC) -c Stack.f90 $(flags)
