import sys, os
from lib import *
import numpy as NP
import time

import wx
import wx.stc as stc
import threading
import copy
import matplotlib
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import LogNorm
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.backends.backend_wx import NavigationToolbar2Wx
from mpl_toolkits.axes_grid.anchored_artists import AnchoredText
from collections import deque

from defaults import *
from NewFrame import *
from RightPanel import *
from GraphStack import *
from PipeSim import *
from Signals import *
from Events import *

# TODO:  Use nplots as default window size
# print pyfft1mod.fft1d.pycal()

# Button definitions
ID_START = wx.NewId()
ID_STOP = wx.NewId()

programDefaults = DefaultLoader("foo.default")


# GUI Frame class that spins off the worker thread
class MainFrame(wx.Frame, Dispatcher, DefaultsCommLink):
    """Class MainFrame."""

    def __init__(self, parent, id, loader, pipemode=None, que=None, events=None, outq=None):
        """Create the MainFrame."""
        wx.Frame.__init__(self, parent, id, 'Control Panel',
                          style=wx.DEFAULT_FRAME_STYLE)
        self.loader = loader
        self.loader.loadFromFile()
        self.initStack()

        self.status = self.CreateStatusBar()  # create status bar
        self.status.SetStatusText("Hola")

        self.rpanel = RightPanel(self, self)  # create interface
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(self.rpanel, proportion=1, flag=wx.ALL | wx.EXPAND, border=2)
        self.Bind(wx.EVT_CLOSE, self.OnExit)
        self.SetSizerAndFit(self.sizer)

        # self.Bind(wx.EVT_CLOSE, self.OnQuit)

        self.pEvents = events  # Communicate with parent process, various GUI events
        self.outq = outq
        self.pipemode = pipemode
        self.commKill = (pipemode, que, None, events, outq)
        self.worker = PipeSimulation(self, pipemode, que)
        EVT_RESULT(self, self.OnResultPre)  # Link up events
        EVT_CONTROL(self, self.OnControl)
        EVT_NEWTIME(self, self.OnNewTime)  # link up sim time
        EVT_CLEARGRAPHSTACK(self, self.OnClearGraphStack)
        self.windowList = []  # list of frames
        self.InitMenu()
        self.Bind(wx.EVT_ACTIVATE, self.OnFocus) # We rebuild the menu every focus to deal with Mac
        self.Show(True)

    def OnFocus(self, event):
        wx.CallAfter(self.InitMenu)

    def InitMenu(self):
        menubar = wx.MenuBar()
        fileMenu = wx.Menu()
        fitem = fileMenu.Append(wx.ID_EXIT, 'Quit', 'Exit Application')
        menubar.Append(fileMenu, '&File')
        self.SetMenuBar(menubar)
        self.Bind(wx.EVT_MENU, self.OnExit, fitem)

    def OnExit(self, event):
        self.pEvents.put(ExitSignal())  # Tell main thread to exit
        wx.CallAfter(self.rpanel.OnStart, None)  # Call run to pump event loop

    # EVT_RUNSTEP(self.rpanel,self.OnExitPhase2)  #Redirect running loop to an exit callback

    def OnExitPhase2(self, event):
        self.Destroy()

    def OnNewTime(self, event):
        if self.worker.simdata.has_key('tend'):
            te = self.worker.simdata['tend']
            self.rpanel.timerText.SetLabel("Time is " + str(event.time) + " of " + str(te))
        else:
            self.rpanel.timerText.SetLabel("Time is " + str(event.time))
        self.rpanel.displayTime = float(event.time)

    def OnReset(self, event):
        self.pEvents.put(ResetSignal())
        wx.CallAfter(self.rpanel.OnStart, None)  # Run one time step is necessary to reset

    def OnControl(self, event):
        if event.data.signame == "OPENFRAME":
            self.rpanel.makeNewFrame(event.data.layout, event.data.defaults)
        elif event.data.signame == "NEWDYNAMICVAR":
            print "New Var " + event.data.varname
            self.rpanel.realTimeVars.append(event.data.varname)

    def OnResultPre(self, event):
        # Find a home for the event
        # Do not pass raw wxPython result
        self.OnResult(CopyResultEvent(event))  # Call the result handler in the Dispatcher mixin

    def OnClearGraphStack(self, event):
        if hasattr(event, "codename"):
            self.dispatchers.append(GraphStack(3, event.codename, event.desc, callback=self))
        else:
            del self.dispatchers[:]  # Delete all objects in dispatchers, defined in GraphStack.py

    def GraphStackChanged(self, num, name):
        self.outq[name] = num  # Tell the manager to change


def restart_program():
    """Restarts the current program.
    Note: this function does not return. Any cleanup action (like
    saving data) must be done before calling this function."""
    python = sys.executable
    os.execl(python, python, *sys.argv)


class MainApp(wx.App):
    """Class Main App."""

    def __init__(self, arg, pipemode=None, que=None, timedir=None, events=None, outq=None):
        self.pipemode = pipemode
        self.que = que
        self.pEvents = events
        self.outq = outq
        wx.App.__init__(self, arg)

    def OnInit(self):
        """Init Main App."""
        self.frame = MainFrame(None, -1, programDefaults, pipemode=self.pipemode, que=self.que, \
                                       events=self.pEvents, outq=self.outq)
        self.frame.Show(True)
        self.SetTopWindow(self.frame)
        return True


if __name__ == "__main__":
    # main()
    app = MainApp(0)
    app.MainLoop()
