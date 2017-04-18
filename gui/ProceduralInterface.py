import numpy as np
import wx
import os
import time
from threading import Thread
from multiprocessing import Process, Pipe, Queue, Lock, Value, Manager
import cPickle

import Graphs
import GraphStack
import NewFrame
from Events import *
from defaults import *
from Signals import *

import new_beps1gl


class DispItem:
    def __init__(self, value, priority):
        self.value = value
        self.priority = priority

    def __cmp__(self, other):
        return self.priority.__cmp__(other.priority)


# Try the high level interface
def initGui(q, que, td, events, outqueue):
    if wx.GetApp() == None:
        app = new_beps1gl.MainApp(0, q, que, td, events=events, outq=outqueue)
        app.MainLoop()
        return app
    else:
        print "Cannot create multiple wxApp contexts"
        return False


class PlasmaContext():
    def __init__(self, *args):
        assert(len(args) == 6)
        self.defaultGraphs = []
        self.norun = False
        #read in comm channels
        self.async = args[5]  # synch or async mode
        self.que = args[2]
        self.events = args[4]
        self.timeDir = args[3] 
        self.parent_conn = args[1]
        self.child_conn = args[0]
        
        self.curTime = 0
        self.graphEnabled = True
        self.callbacks = dict()

    @staticmethod
    def runMain(func):
        #create bidirectional comm channels
        manager = Manager()
        async = manager.dict()  # synch or async mode
        que = Queue()
        events = Queue()
        timeDir = None 
        child_conn = Queue()
        #run the simulation code in child process
        p = Process(target=func, args=(child_conn, child_conn, que, timeDir, events, async))
        #p.daemon = True
        p.start()
        #run gui in parent process
        initGui(child_conn, que, timeDir, events, async)


    # Low Level method to communicate with gui thread
    def _sendplot(self, obj):
        if self.norun:
            return
        if self.graphEnabled:
            try:
                obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
            except:
                True
            self.que.put(cPickle.dumps(obj))
            self.child_conn.get()

    def _sendplotasync(self, obj):
        if self.norun:
            return
        self.que.put(cPickle.dumps(obj))

    # Takes a layout index between 1 and 4, and a list of default graphs to plot
    def newFrame(self, layout, defaults):
        if self.norun:
            return
        to = OpenFrame()
        to.layout = layout
        to.defaults = defaults
        self.que.put(cPickle.dumps(to))

    def runOnce(self):
        #This really should directly press the step button
        if self.norun:
            return
        self._sendplot("RUNONCE")

    # process events, such as callbacks and variable changes from the GUI
    def getEvents(self, obj, pause=True):
        if self.norun:
            return
        que = []
        readQ = True

        if pause:
            self._sendplot("RUNCONTROL")  # This simply makes sure the run once button runs only 1 timestep

        while readQ:
            try:
                to = self.events.get(not pause)
                que.append(to)
            except:
                readQ = False

        # Run callbacks.  A signal sent from the GUI must have a signame attribute
        for q in que:
            if self.callbacks.has_key(q.signame):
                if q.signame == "EXIT":
                    self._sendplot("EXIT")
                    self.showGraphs(False)
                    self.norun = True
                cb = self.callbacks[q.signame]
                cb(obj, to)

    def exit(self):
        self.child_conn.close()
        self.que.close()
        self.que.join_thread()
        self.p.join()

    # Enable or disable graphics for speed reasons
    def showGraphs(self, val):
        if self.norun:
            return
        self.graphEnabled = val

    def isGraphing(self, name):
        if self.norun:
            return
        if self.graphEnabled == False:
            return False
        if name != None:
            try:
                if self.async[name] == 0:
                    return False
            except:  # Key does not exist.  Not drawing
                return False
        return self.graphEnabled

    # Set aync or sync mode
    # def asyncMode(self,mode):
    #   self.async.value = mode

    # Set the global time in the control panel
    def setTime(self, time):
        if self.norun:
            return
        # Let the graphs know the simulation time
        self.curTime = time
        obj = SetFrameTime(time)
        obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
        self.que.put(cPickle.dumps(obj))

    def pause(self):
        if self.norun:
            return
        self._sendplotasync("PAUSE")

    # Allow fast forwarding.  Stop sending graphics output when ctime is less than some value specified in
    # inputlist variable fastforward
    def fastForward(self, ctime, obj):
        global _ffwding
        if self.norun:
            return
        if hasattr(obj, "fastforward"):
            if ctime < obj.fastforward:
                self.showGraphs(False)
                _ffwding = True
            else:
                try:
                    _ffwding
                except:
                    _ffwding = False
                if _ffwding == True:
                    self.pause()
                    _ffwding = False
                self.showGraphs(True)
        else:
            return

    # Plottype is optional.  Use it to rename the dv1 plottype
    def showVelocity(self, data, labels, fvm=None, plottype=None):
        if self.norun:
            return
        pt = plottype
        if pt == None:
            pt = "DRAWVELOCITY"
        if not self.isGraphing(pt):
            return
        dv1 = Graphs.DrawVelocity(data, labels, fvm=fvm)
        if plottype is not None:
            dv1.plottype = plottype
        self._sendplot(dv1)

    def showPotential(self, data):
        if self.norun:
            return
        if not self.isGraphing("DRAWPOT"):
            return
        dv1 = Graphs.DrawPotential(data)
        self._sendplot(dv1)

    def showEnergy(self, time, data, maxtimeindex, labels):
        if self.norun:
            return
        if not self.isGraphing("ENERGY"):
            return
        dv1 = Graphs.DrawEnergy(data, time, labels, timeindex=maxtimeindex)
        self._sendplot(dv1)

    def showSimple(self, name, xdata, ydata, text, graphoptions=None):
        if self.norun:
            return
        if not self.isGraphing(name[0]):
            return
        dv1 = Graphs.DrawSimple(name, xdata, ydata, text, graphoptions=graphoptions)
        self._sendplot(dv1)

    def showPhase(self, ppart, kpic,
                  plottype=None):  # data is the particle data, ppart.  kpic is array of num particles per tile
        if self.norun:
            return
        pt = plottype
        if pt == None:
            pt = "DRAWPHASE"
        if not self.isGraphing(pt):
            return
        # shape is the bounds of the histogram, [[xmin,xmax], [ymin,ymax]]
        numPart = np.sum(kpic)  # number of particles
        numTiles = np.size(kpic)  # number of tiles

        xvInTile = np.zeros((2, numPart), ppart.dtype)
        isum = 0
        for k, kk in enumerate(kpic):  # De-tile particles in to one big fat array
            xvInTile[0, isum:kk + isum] = ppart[0, 0:kk, k]
            xvInTile[1, isum:kk + isum] = ppart[1, 0:kk, k]
            isum += kk

        dv1 = Graphs.DrawPhase(xvInTile)  # copy the data
        if plottype != None:
            dv1.plottype = plottype
        self._sendplot(dv1)

    def showFastPhase(self, ppart, kpic):  # data is the particle data, ppart.  kpic is array of num particles per tile
        if self.norun:
            return
        if not self.isGraphing("DRAWFASTPHASE"):
            return
        # shape is the bounds of the histogram, [[xmin,xmax], [ymin,ymax]]
        numPart = np.sum(kpic)  # number of particles
        numTiles = np.size(kpic)  # number of tiles

        xvInTile = np.zeros((2, numPart), ppart.dtype)
        isum = 0
        for k, kk in enumerate(kpic):  # De-tile particles in to one big fat array
            xvInTile[0, isum:kk + isum] = ppart[0, 0:kk, k]
            xvInTile[1, isum:kk + isum] = ppart[1, 0:kk, k]
            isum += kk

        H, x, y = np.histogram2d(xvInTile[0], xvInTile[1], bins=40)

        dv1 = Graphs.DrawFastPhase(x, y, H)  # copy the data
        self._sendplot(dv1)

    def showTrajectory(self, data):
        if not self.isGraphing("DRAWTRAJ"):
            return
        dv1 = Graphs.DrawTrajectory(data)
        self._sendplot(dv1)

    def showDensity(self, data, nx):
        if not self.isGraphing():
            return
        dv1 = Graphs.DrawElectronDensity(data, nx)
        self._sendplot(dv1)

    def showScaler(self, data, name, nx, time):
        if not self.isGraphing("DRAWDENSE"):
            return
        dv1 = Graphs.DrawScaler(name, data, nx, time)
        self._sendplot(dv1)

    def showPhi(self, time, phi, dta, dt, plottype=None, omn=100):
        if not self.isGraphing("DRAWPHI"):
            return
        if len(phi) > 0:
            dv1 = Graphs.DrawPhi([np.array(time), phi, dta], dt, omn=omn)
            if plottype != None:
                dv1.plottype = plottype
            self._sendplot(dv1)

    def showSimpleImage(self, name, data, text, extent=(), labl=("", "")):
        if not self.isGraphing(name):
            return
        dv1 = Graphs.DrawSimpleImage(name, data, text, extent=extent, labl=labl)
        self._sendplot(dv1)

    def showMultiTrajectory(self, partd, itt, comp):
        if not self.isGraphing("DRAWMULTITRAJ"):
            return
        dv1 = Graphs.DrawMultiTraj(partd, itt, comp)
        self._sendplot(dv1)

    def wait(self, obj):
        #   self.asyncMode(1)
        while True:
            self.getEvents(obj)
            time.sleep(1.0 / 30.0)

    def clearGraphList(self):
        if self.norun:
            return
        ptr = ClearGraphStack()
        self.que.put(cPickle.dumps(ptr))

    def updateSimInfo(self, data):
        if self.norun:
            return
        ptr = SimData(data)
        self.que.put(cPickle.dumps(ptr))

    def addGraph(self, codename, desc, autoadd=True, priority=100):
        if self.norun:
            return
        ptr = ClearGraphStack()
        ptr.codename = codename
        ptr.desc = desc
        if autoadd:
            self.defaultGraphs.append(DispItem(codename, priority))
        self.que.put(cPickle.dumps(ptr))

    def RunNow(self, state):
        if self.norun:
            return
        ptr = RunNow(state)
        self.que.put(cPickle.dumps(ptr))
