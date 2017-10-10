# Installation

Python_PIC_Gui will run on all major platforms that support python, however Windows is not currently officially supported.  Mac OS X or Linux are recommended. 

# Requirements

There are four required packages to run the software, and the version is very important.  Numpy greater than version 1.11 is incompatible.

* numpy 1.7.1 to 1.11 (1.13 or greater does not work)
* matplotlib 2.0.2 
* wxPython 3.0.0 or 4.0.0b1 (anything larger does not work)
* f90nml
* ffmpeg (optional)

To compile the Fortran backend a fortran compiler such as gfortran and a c compiler are recommended.  We use gcc or clang on apple.

## Installation on OS X

Python_PIC_Gui should run well on OS X, either the native python or Anaconda.  We provide install instructions for both (the native python version looks slightly nicer).  To avoid conflicts with any versions python packages currently installed, we recommend installing in a virtual environment.

### Install using Apple's Python

Check if virtualenv is installed by typing in Terminal

    virtualenv
    
If the command was found, move on.  Otherwise install virtualenv.  We recommend using easy_install

    sudo easy_install virtualenv
    
The native install instructions should work even if you have Anaconda installed. However, if Anaconda installed a version of virtualenv, you cannot use it for the native install!  The work around is to replace `virtualenv` with `/usr/local/bin/virtualenv` everywhere in this section of the install guide.  You can check the version by typing `which virtualenv`.
    
The above command should be the only time root access is required, however virtualenv can be installed by other means without root (we do not decribe them here).

Now download the software:

    git clone https://github.com/starside/Python_PIC_Gui
    cd Python_PIC_Gui
    
Create the and activate the virtualenv

    virtualenv ENV --python=/usr/bin/python
    source ENV/bin/activate
    
The `--python=/usr/bin/python` tells the virtualenv to use Apple's python even if other version of python are installed.  Now install the dependencies.

    pip install --upgrade pip
    pip install -r requirements.txt
    
Now build the Fortran.  Note you must perform the steps above first, as the Fortran build process requires numpy.

    make python
    
Almost everything is in place.  On Apple, a special version of python is required to access the GUI.  Copying Apple's `pythonw` to our virtualenv works.

    cp /usr/bin/pythonw ENV/bin/python

To run the application (for example the Electrostatic code) type

    python gui_mbeps1.py
    
If all went well, the application should appear.

To exit the virtual environment type `deactivate`.  Later, if you want run the PIC gui simple cd to the Python_PIC_Gui directory and run `source ENV/bin/activate`.  Below is the whole sequence of commands, that activates the virtual environment, runs the application, and then deactivates virtualenv.

    cd Python_PIC_Gui
    source ENV/bin/activate
    python gui_mbeps1.py
    deactivate
    
## Installing on Anaconda
Installation on Anaconda should work for both Mac OS X and Linux.  It will probably work for Windows if you can figure out how to compile the Fortran.

### Disable OpenMP
In the file mbeps1.source/Makefile disable OpenMP.  For whatever reason, Anaconda's numpy does not correctly compile with OpenMP support.  

Comment out lines 78 and 79 in mbeps1.source/Makefile then comment in 81 and 82.

### Simple Method
Install the requirements listed in the Requirements section.  You will probably have to downgrade numpy to 1.11 or lower (If you do not want to downgrade, read the section below).  Anaconda has a version of wxPython (3.0.0).  On Apple, wxPython should also install python.app, if not you should install it.  Once everything is installed type

    cd Python_PIC_Gui
    make python
    
On Apple, run the application by typing
    
    python.app gui_mbeps1.py
    
Using `python.app` is very important, otherwise the GUI will fail to start. 

On Linux you can simply type 
    python gui_mbeps1.py
    
### Anaconda Virtual Environment

Anaconda supports its own virtual environment.  This section describes how to install in an Anaconda virtual environment so you do not have to downgrade numpy or other packages.

Download the code then create and activate the virtual environment

    git clone https://github.com/starside/Python_PIC_Gui
    cd Python_PIC_Gui
    conda create -n ENV python=2.7
    source activate ENV
    
Install required packages

    conda install -n ENV "numpy<=1.11"
    conda install -n ENV wxpython
    conda install -n ENV matplotlib
    pip install f90nml
    
Disable OpenMP in the mbeps1.source/Makefile and run  

    make python
    
 To run the application type
 
    python.app gui_mbeps1.py
   
    
## Installation on Linux

The installation process on Linux is the similar to the direction on OS X.  Using Anacondia the procedure should be identical to OS X, except you do not need to do the step of copying pythonw, or running with python.app.  

For non-Anaconda installation, the procedure should follow the Apple Native install instructions (read it!), except Linux does not use easy_install.  Instead use your distro's package manager.  On Ubuntu, installing virtualenv would look like 

    sudo apt-get install virtualenv
    
The only tricky part is getting wxPython.  Pip will likely try to build it from source (which will almost certainly fail), so remove wxPython from requirements.txt and see if you can find a binary install of wxPython for your distribution.  wxPython provides binary packages for some platforms

https://extras.wxpython.org/wxPython4/extras/linux/

You can also use wxPython 3.0, however it will not install in a virtualenv, so you will have to install everything else outside of a virtualenv.

For Ubuntu, and other Linux distros you can try installing the dependencies with

     pip install -r requirements_ununtu1604.txt
     
The file `requirements_ununtu1604.txt` can be modified to install any binary version of wxPython in https://extras.wxpython.org/wxPython4/extras/linux/ to suit your distro.
    
    
    
  
