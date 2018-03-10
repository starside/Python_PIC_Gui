from types import *
import numpy as np
import wx
import os
import sys
import time
from threading import Thread
from multiprocessing import Process, Pipe, Queue, Lock, Value, Manager
import cPickle
from collections import namedtuple

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
def initGui(q, que, events, outqueue):
    if wx.GetApp() == None:
        app = new_beps1gl.MainApp(0, q, que, events=events, outq=outqueue)
        app.MainLoop()
        return app
    else:
        print "Cannot create multiple wxApp contexts"
        return False

Connection = namedtuple("Connection", "gui_conn, que, events, async")

##PlasmaContext is the interface the sim process uses to talk to the GUI.
class PlasmaContext():
    """
        Constructor for an interface object.  Only one should be needed per
        application.  The returned object has all the required methods for
        communicating with the GUI.

        :param obj: Container object, this can be anything.  It minimally must
            have two attributes dt and tend, both floats.  dt is a time step
            and tend is end time of the simulation.  For example, dt could be
            0.1 and tend might be 100.0
        :param args*: Parameters passed to main(args*).  See Tutorials 1 for
            an example of useage. 
    """
    def __init__(self, obj, *args):
        assert(len(args) == 4)
        self.simObj = obj
        self.defaultGraphs = []
        self.norun = False
        #read in comm channels
        self.conn = Connection(*args) # This is a bundle of connections to the simulation process
        
        self.curTime = 0
        self.graphEnabled = True
        self.callbacks = dict()
        self.graphEarly = dict()    #dict of plots to plot before end of fastforarding
        #Set default callbacks
        self.callbacks["VARCHANGE"] = changeVarsCallback  # Set a callback
        self.callbacks["RESET"] = resetCallback
        self.callbacks["EXIT"] = exitCallback

    @staticmethod
    def runMain(func):
        """
        This function launches the main simulation function in a child process.
        This is necessary, becase on OS X for whatever reason the GUI fails to
        respond if launched in the child process.

        :param func: The name of a function, which should take a single parameter.
            From the host process you would call PlasmaContext.runMain(main). 
            main would be defined with prototype main(args*). main(args*) will
            call something like: pc = PlasmaContext(in1,*args) to create an
            interface object.

        """
        manager = Manager()
        # async is a multiprocess dictionary used to keep track of which plots are being
        # displayed, this way the simulation process does not need to generate graphs for
        # invisible plots
        async = manager.dict()  # sync or async mode
        # que is the primary means of sending plot data to the GUI thread, typically using
        # _sendplot
        que = Queue()
        # events queue is used to signal events from the GUI to the sim process.  It is
        # Not bidirectional.  The events are generated in getEvents.
        events = Queue()
        # gui_conn queue is used to recieve messages from the gui.  Currently it is only
        # used to keep the simulation in sync with the GUI.  After a plot is sent using
        # _sendplot to the gui, _sendplot will pause the simulation process and wait for
        # any response on gui_conn before proceeding.  This is perhaps a heavy handed way to
        # keep the display and simulation in sync
        gui_conn = Queue()
        # Wrap the connection elements in to a namedtuple to send to the new process
        # A named tuple is used for code clarity
        conn = Connection(gui_conn, que, events, async)
        # run the simulation code in child process
        p = Process(target=func, args=conn)
        #p.daemon = True
        p.start()
        #run gui in parent process
        initGui(gui_conn, que, events, async)

    def _sendEarly(self, obj):
        """
        This is a test to see if the object obj to be sent to the GUI for 
        rendering/processing has been flaged for early plotting.  If so,
        calculate if the sim is close enough to the end of the fast forward
        to plot
        """
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

    def _sendplot(self, obj):
        """Low level method to send an object to the GUI processes via pipe.
           It also serializes the object using cPickle

           :param obj: The object to send.

        """
        if self.norun:
            return
        if self.graphEnabled or self._sendEarly(obj):  #self.graphEarly is set when fastforward is called
            try:
                obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
            except:
                True
            iv = cPickle.dumps(obj)
            self.conn.que.put(iv)
            self.conn.gui_conn.get() # Wait for response.  This will block until response recieved

    def _sendmessageasync(self, obj):
        """
        Sends a plot, asynchronously.  It will not wait for a response before returning.
        """
        if self.norun:
            return
        self.conn.que.put(cPickle.dumps(obj))

    def graphBeforeEndOfFF(self, plottype, interval):
        """
        Tells te system to graph any plot objects of type plottype at time interval # of steps
        before the fastforward ends.  This ensures that even if you fastforward past all of a plots
        graphing times, it will still draw the last one.

        :param plottype: the string name of the plot
        :param interval: how frequently the plot is generated.  In time steps
        """
        self.graphEarly[plottype] = interval

    # Takes a layout index between 1 and 4, and a list of default graphs to plot
    def newFrame(self, layout, defaults):
        """
        Creates a new frame with a specified layout.

        :param layout: The type of layout specified by a string, currently 
            there are four options: "Layout1", "Layout2v","Layout2h",
            "Layout3", and "Layout4".  These correspond to one plot,
            two plots arranged vertically, two plots arranged horizontally,
            three plots and four plots.

        :param defaults: a list of plot names to display.  If layout="Layout3"
            then defaults must have length 3.  The plot names correspond to the
            codename field in addGraph(codename, desc).

        """
        if self.norun:
            return
        to = OpenFrame()
        to.layout = layout
        to.defaults = defaults
        self.conn.que.put(cPickle.dumps(to))

    def newDynamicVariable(self, varname):
        """
        Create a dynamic variable, that can be changed in the simulation loop.  This
        variable will appear as red in the built-in input editor.  When these variables
        are updated they will appear automatically as attributes in the container object
        specified in the constructor of PlasmaContext.  The automatically created field
        will be all lowercase.

        :param varname: A string specifying the name of the dynamic variable as it 
            appears in the editor.  

        """
        if self.norun:
            return
        to = NewDynamicVariable()
        to.varname = varname
        self.conn.que.put(cPickle.dumps(to))

    def runOnce(self):
        #This really should directly press the step button
        if self.norun:
            return
        self._sendplot("RUNONCE")

    # process events, such as callbacks and variable changes from the GUI
    def getEvents(self, pause=True):
        """
        This is an important function, and must be called once per loop of the simulation.
        This function responds to input from the GUI.  If this function is not called, the
        GUI will be unresponsive.
        """
        obj = self.simObj
        if self.norun:
            return
        que = []
        readQ = True

        if pause:
            self._sendplot("RUNCONTROL")  # This simply makes sure the run once button runs only 1 timestep

        while readQ:
            try:
                to = self.conn.events.get(not pause)
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
        self.conn.gui_conn.close()
        self.conn.que.close()
        self.conn.que.join_thread()
        self.p.join()

    # 
    def showGraphs(self, val):
        """ Enable or disable graphics for performance reasons

        :param val: True or False
        """
        if self.norun:
            return
        self.graphEnabled = val

    def isGraphing(self, name):
        if self.norun:
            return
        if name != None:
            try:
                if self.conn.async[name] == 0:
                    return False
            except:  # Key does not exist.  Not drawing
                return False
        return True

    def setTime(self, time, dt):
        """
        Sets the time in the control panel.

        :param time: Simulation time

        :param dt: Time step size

        """
        if self.norun:
            return
        # Let the graphs know the simulation time
        self.curTime = time
        self.dt = dt
        obj = SetFrameTime(time)
        obj._tackOnTime = self.curTime  # Just sloppily glue the time on the object
        self.conn.que.put(cPickle.dumps(obj))

    def pause(self):
        """
        Pauses the simulation
        """
        if self.norun:
            return
        self._sendmessageasync("PAUSE")

    def fastForward(self):
        """
        Allows the simulation to fast forward.  Call immediatly after getEvents()
        """
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
            self.graphBeforeEndOfFF(pt, early)
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
            self.graphBeforeEndOfFF(pt, early)
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
        """
        Call after simulation loop has run.  This command blocks and keeps the GUI responsive
        """
        while True:
            self.getEvents(obj)
            time.sleep(1.0 / 30.0)

    def clearGraphList(self):
        """ Clears the default items in the right-click menu.  This
        is eventually going to be depracated

        """
        if self.norun:
            return
        ptr = ClearGraphStack()
        self.conn.que.put(cPickle.dumps(ptr))

    def updateSimInfo(self, data):
        """
        Updated simulation metadata.  Currently the only field is 'tend', the end time
        of the simulation.  This is done by passing a dictionary with key 'tend' as a 
        string, followed by a float.  For example, {'tend', 100.0}
        """
        if self.norun:
            return
        ptr = SimData(data)
        self.conn.que.put(cPickle.dumps(ptr))

    def addGraph(self, codename, desc, autoadd=True, priority=100):
        """ 
            Add a graph to the menu system.  This is required to
            display a plot

            :param codename: This is the computer's name for a plot.
                It does not have to be human readable, but it never
                hurts to use a name that makes sense.

            :param desc: The human readable name for the plot.  This
                is what will appear in the right-click menu.

            :param autoadd: Controls whether the plot is automatically
                displayed on startup.  If set, a new plotting area will
                be automatically created.

            :param priority: Automatically change order plots are 
                displayed

        """
        if self.norun:
            return
        ptr = ClearGraphStack()
        ptr.codename = codename
        ptr.desc = desc
        if autoadd:
            self.defaultGraphs.append(DispItem(codename, priority))
        self.conn.que.put(cPickle.dumps(ptr))

    def RunNow(self, state):
        if self.norun:
            return
        ptr = RunNow(state)
        self.conn.que.put(cPickle.dumps(ptr))
