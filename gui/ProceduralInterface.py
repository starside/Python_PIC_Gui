from types import *
import numpy as np
import wx
import os
import sys
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

int_type = np.int32
double_type = np.float64
float_type = np.float32
complex_type = np.complex64

#These are callback functions, that the user is free to redefine,
#Which is done directly by setting the appropriate key in
#self.callbacks.  For example:
#self.callbacks["EXIT"] = exitCallback
def changeVarsCallback(obj, to):
    try:
        for key in to.var:
            if key == "fastforward":
                if rightType(to.var[key]) > obj.tend:
                    continue
            setattr(obj, key, rightType(to.var[key]))
    except AttributeError:
        print "Could not change variables"


# The function to be called when reset is pushed
def resetCallback(obj, to):
    print "The reset button was pushed!"
    # Do something
    print obj.tend

#Called in the sim thread on exit.
def exitCallback(obj, to):
    exit(0)


#This is a simple function that takes a number and gives a fortran
#consisten type
def rightType(val):
    ints, reals, complexs = int_type, float_type, complex_type
    if type(val) is IntType:
        return np.array([val], ints)
    elif type(val) is FloatType:
        return np.array([val], reals)
    elif type(val) is ComplexType:
        return np.array([val], complexs)



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

##PlasmaContext is the interface the sim process uses to talk to the GUI.
class PlasmaContext():
    def __init__(self, obj, *args):
        assert(len(args) == 6)
        self.simObj = obj
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
        self.graphEarly = dict()    #dict of plots to plot before end of fastforarding
        #Set default callbacks
        self.callbacks["VARCHANGE"] = changeVarsCallback  # Set a callback
        self.callbacks["RESET"] = resetCallback
        self.callbacks["EXIT"] = exitCallback

    #This function launches the main simulation function in a child process.
    #This is necessary, becase on OS X for whatever reason the GUI fails to
    #respond if launched in the child process
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

    #This is a test to see if the object obj to be sent to the GUI for 
    #rendering/processing has been flaged for early plotting.  If so,
    #calculate if the sim is close enough to the end of the fast forward
    #to plot
    def _sendEarly(self, obj):
        try:
            obj.plottype
        except:
            return False
        try:
            time = self.graphEarly[obj.plottype] #Fire early?
            assert(time >= 0)
            ntime = int(round(self.curTime/self.dt))
            ffs = int(round(self.simObj.fastforward/ self.dt))
            it = int(ffs/ time) #Fast Forard stop divided by the plot frequency
            if ntime >= (it-1)*time:    #Fire Early
                return True
        except:
            return False
        return False


    # Low Level method to communicate with gui thread.  Serializes and sends objects
    def _sendplot(self, obj):
        if self.norun:
            return
        if self.graphEnabled or self._sendEarly(obj):  #self.graphEarly is set when fastforward is called
            try:
                obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
            except:
                True
            iv = cPickle.dumps(obj)
            self.que.put(iv)
            self.child_conn.get()

    def _sendplotasync(self, obj):
        if self.norun:
            return
        self.que.put(cPickle.dumps(obj))

    def graphBeforeEndOfFF(self, plottype, interval):
        """
        Tells te system to graph any plot objects of type plottype at time interval # of steps
        before the fastforward ends.  This ensures that even if you fastforward past all of a plots
        graphing times, it will still draw the last one.
        """
        self.graphEarly[plottype] = interval

    # Takes a layout index between 1 and 4, and a list of default graphs to plot
    def newFrame(self, layout, defaults):
        if self.norun:
            return
        to = OpenFrame()
        to.layout = layout
        to.defaults = defaults
        self.que.put(cPickle.dumps(to))

    # Create a new dynamic variable for the input edit
    def newDynamicVariable(self, varname):
        if self.norun:
            return
        to = NewDynamicVariable()
        to.varname = varname
        self.que.put(cPickle.dumps(to))

    def runOnce(self):
        #This really should directly press the step button
        if self.norun:
            return
        self._sendplot("RUNONCE")

    # process events, such as callbacks and variable changes from the GUI
    def getEvents(self, pause=True):
        obj = self.simObj
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
        if name != None:
            try:
                if self.async[name] == 0:
                    return False
            except:  # Key does not exist.  Not drawing
                return False
        return True

    # Set aync or sync mode
    # def asyncMode(self,mode):
    #   self.async.value = mode

    # Set the global time in the control panel
    def setTime(self, time, dt):
        if self.norun:
            return
        # Let the graphs know the simulation time
        self.curTime = time
        self.dt = dt
        obj = SetFrameTime(time)
        obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
        self.que.put(cPickle.dumps(obj))

    def pause(self):
        if self.norun:
            return
        self._sendplotasync("PAUSE")


    # Allow fast forwarding.  Stop sending graphics output when ctime is less than some value specified in
    # inputlist variable fastforward
    def fastForward(self):
        try:
            obj = self.simObj
        except:
            sys.stderr.write('Must call getEvents before fastforward')
            exit(0)
        global _ffwding
        if self.norun:
            return
        if hasattr(obj, "fastforward"):
            if np.round(self.curTime/self.dt) + 1 < np.round(obj.fastforward/self.dt):
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

    # Plot a user defined graph
    def showUserDefined(self, name, definedplot, early=None):
        if self.norun:
            return
        if not self.isGraphing(name):
            return
        if early is not None:
            self.graphBeforeEndOfFF(name, early)
        definedplot.plottype = name
        self._sendplot(definedplot)

    # Plottype is optional.  Use it to rename the dv1 plottype
    def showVelocity(self, data, labels, fvm=None, plottype=None, title=None, early=None):
        if self.norun:
            return
        pt = plottype
        if pt == None:
            pt = "DRAWVELOCITY"
        if not self.isGraphing(pt):
            return
        if early is not None:
            self.graphBeforeEndOfFF("DRAWVELOCITY", early)
        dv1 = Graphs.DrawVelocity(data, labels, fvm=fvm, title=title)
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

    def showEnergy(self, time, data, maxtimeindex, labels, title=None, early=None):
        if self.norun:
            return
        if not self.isGraphing("ENERGY"):
            return
        if early is not None:
            self.graphBeforeEndOfFF("ENERGY", early)
        dv1 = Graphs.DrawEnergy(data, time, labels, timeindex=maxtimeindex, title=title)
        self._sendplot(dv1)

    def showSimple(self, name, xdata, ydata, text, graphoptions=None, title=None, early=None):
        if self.norun:
            return
        if not self.isGraphing(name[0]):
            return
        if early is not None:
            self.graphBeforeEndOfFF(name[0], early)
        dv1 = Graphs.DrawSimple(name, xdata, ydata, text, graphoptions=graphoptions, title=title)
        self._sendplot(dv1)

    def showPhase(self, ppart, kpic,
                  plottype=None, title=None, early=None, twophase=None,
                  plotlabels=["Position", "Velocity"]):  # data is the particle data, ppart.  kpic is array of num particles per tile
        if self.norun:
            return
        pt = plottype
        if pt == None:
            pt = "DRAWPHASE"
        if not self.isGraphing(pt):
            return
        if early is not None:
            self.graphBeforeEndOfFF(plottype, early)
        # shape is the bounds of the histogram, [[xmin,xmax], [ymin,ymax]]
        numPart = np.sum(kpic)  # number of particles
        numTiles = np.size(kpic)  # number of tiles

        xvInTile = np.zeros((2 + 1, numPart), ppart.dtype)
        isum = 0
        for k, kk in enumerate(kpic):  # De-tile particles in to one big fat array
            xvInTile[0, isum:kk + isum] = ppart[0, 0:kk, k]
            xvInTile[1, isum:kk + isum] = ppart[1, 0:kk, k]
            if twophase is not None:
                xvInTile[twophase, isum:kk + isum] = ppart[twophase, 0:kk, k]
            isum += kk

        dv1 = Graphs.DrawPhase(xvInTile, title=title, twophase=twophase, 
                plotlabels=plotlabels)  # copy the data
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

    def showMultiTrajectories(self, name, data, itt, t0, dt, defaultplot, graphoptions=None, title=None, early=None):
        if self.norun:
            return
        if not self.isGraphing(name):
            return
        if early is not None:
            self.graphBeforeEndOfFF(name, early)
        dv1 = Graphs.DrawMultipleTrajectories(name, data, itt, t0, dt, defaultplot, graphoptions=graphoptions, title=title)
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

    def showPhi(self, time, phi, dta, dt, plottype=None, omn=100, title=None):
        if not self.isGraphing("DRAWPHI"):
            return
        if len(phi) > 0:
            dv1 = Graphs.DrawPhi([np.array(time), phi, dta], dt, omn=omn, title=title)
            if plottype != None:
                dv1.plottype = plottype
            self._sendplot(dv1)

    def showSimpleImage(self, name, data, text, extent=(), labl=("", ""), title=None, early=None,ticks_scale=None, norm='Log'):
        if not self.isGraphing(name):
            return
        if early is not None:
            self.graphBeforeEndOfFF(name, early)
        dv1 = Graphs.DrawSimpleImage(name, data, text, extent=extent, labl=labl, title=title, ticks_scale=ticks_scale, norm=norm)
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
