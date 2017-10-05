# Installation

Python_PIC_Gui will run on all major platforms that support python, however Windows is not currently officially supported.  Mac OS X or Linux are recommended. 

# Requirements

There are four required packages to run the software, and the version is very important.  Numpy greater than version 1.11 is incompatible.

numpy 1.7.1 to 1.11 (1.13 or greater does not work)
matplotlib 2.0.2 
wxPython 3.0.0 or 4.0.0a2 (anything larger does not work)
f90nml
ffmpeg (optional)

To compile the Fortran backend a fortran compiler such as gfortran and a c compiler are recommended.  We use gcc or clang on apple.

## Installation on OS X

Python_PIC_Gui should run well on OS X, either the native python or Anaconda.  We provide install instructions for both (the native python version looks slightly nicer).  To avoid conflicts with any versions python packages currently installed, we recommend installing in a virtual environment.

### 
