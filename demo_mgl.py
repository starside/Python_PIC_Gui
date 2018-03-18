# -----------------------------------------------------------------------
# 1D Electrostatic OpenMP PIC code
# written by Viktor K. Decyk and Joshua Kelly, UCLA
# copyright 2016, regents of the university of california
import sys
import math
import numpy
from types import *  # This is required for the rightType function
from mathgl import *
"""
This imports the gui code
"""
sys.path.append('./gui')
from ProceduralInterface import *
import Graphs

class Empty:
    def __init__(self):
        self.dt = 0.1
        self.tend = 1000

class DrawSineMpl(Graphs.AutoMenuBase):
    def __init__(self, xax, yax):
        self.xax = xax
        self.yax = yax
        self.m_Title = "Sine Curve Matplotlib"
        self.m_Amplitude = 1.0

    def drawPlot(self, fig, axes):
        axes.plot(self.xax, self.m_Amplitude*self.yax)
        axes.set_title(self.m_Title)

class DrawSine(Graphs.AutoMenuBase):
    def __init__(self, xax, yax):
        self.context_type = "mathgl"
        self.xax = xax
        self.yax = yax
        self.m_Title = "Sine Curve Mathgl"
        self.m_Amplitude = 1.0

    def drawPlot(self, graph):
        x = mglData(self.xax.view(type=np.matrix))
        y = mglData((self.m_Amplitude*self.yax).view(type=np.matrix))
        graph.SetRange('x',self.xax[0], self.xax[-1])
        graph.Box()
        graph.Axis()
        graph.Title(self.m_Title)
        graph.Plot(y)


"""
Define function that initializes menus
"""
def initialize_menus(pc):
    pc.addGraph("SineMpl", "Sine Function Matplotlib")
    pc.addGraph("Sine", "Sine Function Mathgl")

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
    pc.newFrame("Layout1", ["Sine"])
    # sends data the GUI may want to know about the simulation
    pc.updateSimInfo({"tend": in1.tend})    #End time of the simulation
    #
    # * * * start main iteration loop * * *
    nstart = 0
    nloop = 10000
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
        xax = numpy.linspace(curtime, curtime + 15, 5000)
        yax = numpy.sin(xax)
        dv1 = DrawSineMpl(xax, yax)
        dv2 = DrawSine(xax, yax)
        pc.showUserDefined("SineMpl", dv1, early=1)
        pc.showUserDefined("Sine", dv2, early=1)
    #Keep GUI responsive
    pc.wait(in1)

if __name__ == "__main__":
    PlasmaContext.runMain(main)
