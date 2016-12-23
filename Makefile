#Makefile for 1D OpenMP PIC codes in mbeps1.source

# Linkage rules

all : python

fortran : fbuild
	make cpexecs -C mbeps1.source

fbuild :
	make -C mbeps1.source

python :
	make python -C mbeps1.source

clean :
	make clean -C mbeps1.source

clobber: rclobber
	rm -f mbeps1 mbbeps1 mdbeps1

rclobber:
	make clobber -C mbeps1.source
