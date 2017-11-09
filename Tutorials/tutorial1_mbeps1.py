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

"""
Define function that initializes menus
"""
def initialize_menus(pc):
    pc.addGraph("NOPLOT", "No Plot")


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
    pc.newFrame("Layout1", ["NOPLOT"])
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
        pc.getEvents()  #Waits here for GUI
        pc.fastForward()

    #Keep GUI responsive
    pc.wait(in1)

if __name__ == "__main__":
    PlasmaContext.runMain(main)
