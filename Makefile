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

UNAME_S = $(shell uname -s)
ifeq ($(UNAME_S), Darwin)
MACVER=$(shell python macplatform.py)
endif

mac : python
	py2applet --make-setup macLoader.py
	python setup.py py2app
	mkdir dist/macLoader.app/Contents/Resources/deps/
	mkdir dist/macLoader.app/Contents/Resources/deps/$(MACVER)
	mkdir dist/macLoader.app/Contents/Resources/deps/runtime
	mkdir dist/macLoader.app/Contents/Resources/deps/$(MACVER)/runtime
	cp mbeps1.source/*.so dist/macLoader.app/Contents/Resources/deps/
	cp mbeps1.source/*.so dist/macLoader.app/Contents/Resources/deps/$(MACVER)
	python checkdepends.py dist/macLoader.app/Contents/Resources/deps/$(MACVER)/runtime
	python checkdepends.py dist/macLoader.app/Contents/Resources/deps/runtime
	cp -R dist/macLoader.app .
	
