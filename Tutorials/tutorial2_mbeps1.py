# -----------------------------------------------------------------------
# 1D Electrostatic OpenMP PIC code
# written by Viktor K. Decyk and Joshua Kelly, UCLA
# copyright 2016, regents of the university of california
import sys
import math
import numpy
from types import *  # This is required for the rightType function

"""
This imports the gui code
"""
sys.path.append('./gui')
from ProceduralInterface import *

class Empty:
    def __init__(self):
        self.dt = 0.1
        self.tend = 100

class DrawWeierstrass():
    def __init__(self, numTerms, a, b):
        self.numTerms = int(numTerms)
        self.a = a
        self.b = int(b)

    def drawPlot(self, fig, axes):
        #define weierstrass function
        def fweier(x):
            return an.dot(numpy.cos(bn*numpy.pi*x))

        condition = 1.0 + 1.5*numpy.pi
        if self.numTerms < 1:
            print("Number of terms must be greater than 1 and an integer")
            return
        if numpy.abs(self.a) > 1 or self.b < 1 or self.a < 0:
            print("Not met condition 0 < a < 1")
            return
        if self.b%2 == 0:
            print("b must be odd and positive")
            return
        if self.a*self.b <= condition:            
            print("And a*b > 1 + (3/2)*pi")
            return
        n = numpy.array(range(self.numTerms)) # Generate exponents from 0 to numTerms - 1
        bn = numpy.power(self.b, n) # Raise b to powers of n
        an = numpy.power(self.a, n) # Raise a to powers of n
        xax = numpy.linspace(0,0.1,100000)
        yax = numpy.array([fweier(x) for x in xax])
        axes.plot(xax, yax)

"""
Define function that initializes menus
"""
def initialize_menus(pc):
    pc.addGraph("NOPLOT", "No Plot")
    pc.addGraph("weierstrass", "Weierstrass function")


def main(*args):
    # Create simulation container
    in1 = Empty()
    # init GUI
    pc = PlasmaContext(in1, *args)  # Create GUI
    pc.showGraphs(True)  # enable graphics.  Setting to false will disable graphics
    pc.clearGraphList()  # remove all default graph options
    """
    Initialize default windows
    """
    initialize_menus(pc)
    pc.newDynamicVariable("NTP")
    pc.newDynamicVariable("JOSH")
    pc.newFrame("Layout2v", ["NOPLOT","weierstrass"])
    # sends data the GUI may want to know about the simulation
    pc.updateSimInfo({"tend": in1.tend})    #End time of the simulation
    #
    # * * * start main iteration loop * * *
    nstart = 0
    nloop = 100
    for ntime in xrange(nstart, nloop):
        print "ntime = ", ntime
        curtime = ntime * in1.dt
        if ntime == nstart:
            pc.runOnce()
        pc.setTime(curtime, in1.dt)
        if hasattr(in1, 'ntp'):
            print in1.ntp
        if hasattr(in1, 'josh'):
            print in1.josh
        pc.getEvents()  #Waits here for GUI
        pc.fastForward()

        # Draw plots
        dv1 = DrawWeierstrass(ntime + 1, 0.8, 9)
        pc.showUserDefined("weierstrass", dv1, early=1)

    #Keep GUI responsive
    pc.wait(in1)

if __name__ == "__main__":
    PlasmaContext.runMain(main)
