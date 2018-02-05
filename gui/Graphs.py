import math
import copy
import wx
import collections
import functools
import operator
import wx.stc as stc
import numpy as np
import matplotlib
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import LogNorm
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.backends.backend_wx import NavigationToolbar2Wx
from mpl_toolkits.axes_grid.anchored_artists import AnchoredText

import AutoMenu
from Events import *
from lib import *

class KeyList:
    # set the location of the dictionary
    def setDict(self, curdict):
        self._dc = curdict

    def setupKeylist(self, keylist):
        self.keyList = copy.deepcopy(keylist)

        # recieve external parameters from panel

    def setParams(self, val):
        self.setDict(val)

        # delete old keys, then load parameters from dictionary

    def updateKeylist(self, keylist):
        # first remove old keys from local context.  Cleanliness reasons
        for key in self.keyList:
            try:
                delattr(self, "P" + key[0])
            except AttributeError:
                True
        self.keyList = keylist  # set to new keylist
        self.getParameterValues()
        self.setParameters()

    def getParameterValues(self):
        dc = self._dc
        for key in self.keyList:
            if dc.has_key(key[0]):  # set to value in dictionary
                setattr(self, "P" + key[0], dc[key[0]])
            else:  # set default value
                setattr(self, "P" + key[0], key[1])

                # Load parameters in to object attributes

    def setParameters(self):
        dc = self._dc
        for key in self.keyList:
            dc[key[0]] = getattr(self, "P" + key[0])

            # ensures local variables and parameters match

    def syncParameters(self):
        self.getParameterValues()
        self.setParameters()

# Subclass to make new control panels
class ExperimentalBaseControlPanel(wx.Frame):
    def __init__(self, obj, parent):  # remember to delete self at some point
        """Create the MainFrame."""
        wx.Frame.__init__(self, parent, style=wx.FRAME_TOOL_WINDOW | wx.STAY_ON_TOP | wx.SYSTEM_MENU | wx.CLOSE_BOX)
        self.stf = parent
        self.SetPosition((200, 200))
        self.panel = wx.Panel(self, -1, wx.DefaultPosition, wx.DefaultSize)
        self.Bind(wx.EVT_CLOSE, self.OnQuit)
        self.obj = obj

    def OnQuit(self, event):
        wx.PostEvent(self.stf, CloseOptionsEvent())

    def broadcastRedraw(self):
        """ Tells the GUI to refresh the graph in question """
        wx.PostEvent(self.stf, RefreshGraphEvent())

    def loadPersistent(self, key, default):
        """ Loads key from persistent memory in the GraphContainer.
            returns default if it cannot be loaded """
        if key in self.obj._PV:
            return self.obj._PV[key]
        return default

    def setPersistent(self, key, value):
        self.obj._PV[key] = value

    def getPersistentContainer(self):
        return self.obj._PV

class ExperimentalControlPanel(ExperimentalBaseControlPanel):
    def __init__(self, obj, parent):
        ExperimentalBaseControlPanel.__init__(self, obj, parent)
        self.obj = obj
        self.InitUI()
        self.Show(True)

    def UpdateVar(self, key, value):
        """ Set both the persistent value of a variable, and the
            local object's variable """
        setattr(self.obj, key, value) # Set draing object's value
        self.setPersistent(key, value) # Set the persistent value
        self.broadcastRedraw() # Dynamically update values

    def OnChangePlots(self):
        self.OnQuit(self)

    def InitUI(self):
        panel = wx.Panel(self)

        vbox = wx.BoxSizer(wx.HORIZONTAL)

        left = wx.BoxSizer(wx.VERTICAL)
        right = wx.BoxSizer(wx.VERTICAL)

        vbox.Add(left, 1, flag=wx.EXPAND)
        vbox.Add(right, 1, flag=wx.EXPAND)

        # Locate dynamic controls
        # Pass in UpdateVar to let the generated controls know how to update
        # the relevant variables
        controls = AutoMenu.autoGenerateMenu(self.obj, panel, self.UpdateVar)

        # Load values from persistent values.  If persistent variables are not
        # there, create them
        for control in controls:
            # read local value from the object
            local_value = getattr(self.obj, control.key)
            # Load persistent value if possible.  Otherwise use local_value
            new_value = self.loadPersistent(control.key, local_value)
            # Set value in object to new_value
            self.UpdateVar(control.key, new_value)
            # Update the control's view
            control.Update()

        deltaTop = 0
        for c in controls:
            # Calculate label padding
            _, labelY = c.label.GetClientSize()
            _, dataY = c.data.GetClientSize()
            padding = (dataY - labelY)/2.0
            topPad = math.floor(padding) + deltaTop
            bottomPad = math.ceil(padding)
            # Add default spacing
            left.Add((0,7))
            # Add top padding plus accumulated offset
            left.Add((0,topPad))
            left.Add(c.label, flag=wx.ALIGN_RIGHT)
            left.Add((0, bottomPad))
            # Update error offset for top padding
            deltaTop = dataY - (topPad + labelY + bottomPad - deltaTop)

            # Build the right column, add the field
            right.Add((0,7))
            right.Add(c.data)
            c.data.Bind(wx.EVT_TEXT, c.OnEvent)
            c.data.Bind(wx.EVT_KILL_FOCUS, c.OnLostFocus)

        # Add Lower Padding
        lowerPadding = 10
        left.Add((0,lowerPadding))
        right.Add((0,lowerPadding))

        # Fit panel and frames to controls
        panel.SetSizer(vbox)
        panel.Layout()
        panel.Fit()
        self.Fit()

# Subclass to make new control panels
class BaseControlPanel(wx.Frame, KeyList):
    def __init__(self, parent, keylist=[["axesType", "Linear-Linear"]]):  # remember to delete self at some point
        """Create the MainFrame."""
        wx.Frame.__init__(self, parent, style=wx.FRAME_TOOL_WINDOW | wx.STAY_ON_TOP | wx.SYSTEM_MENU | wx.CLOSE_BOX)
        self.keyList = keylist  # parameter name in dictionary + default value
        self.stf = parent
        self.setDict(self.stf.arbGraphParameters)
        self.SetPosition((200, 200))
        self.panel = wx.Panel(self, -1, wx.DefaultPosition, wx.DefaultSize)
        self.updateKeylist(keylist)
        self.syncParameters()
        self.Bind(wx.EVT_CLOSE, self.OnQuit)

    def OnQuit(self, event):
        self.setParameters()
        wx.PostEvent(self.stf, CloseOptionsEvent())

    def broadcastRedraw(self):
        wx.PostEvent(self.stf, RefreshGraphEvent())


class DefaultControlPanel(BaseControlPanel):
    def __init__(self, parent):
        BaseControlPanel.__init__(self, parent)
        vsizer1 = self.SetupControls()
        self.SetSizer(vsizer1)
        self.SetAutoLayout(1)
        vsizer1.Fit(self)
        self.Show()

        # The following methods should be overriden if inherited

    def SetupControls(self):
        vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
        hs1 = wx.BoxSizer(orient=wx.HORIZONTAL)

        self.axesTypeList = wx.Choice(self, -1, choices=["Linear-Linear", "Log-Linear", "Linear-Log", "Log-Log"],
                                        style=wx.CB_READONLY)
        self.axesTypeList.SetStringSelection(self.PaxesType)
        hs1.Add(self.axesTypeList)

        vsizer1.Add(hs1)

        self.axesTypeList.Bind(wx.EVT_CHOICE, self.OnSelect)

        return vsizer1

    def OnSelect(self, event):
        self.stf.arbGraphParameters["axesType"] = event.GetString()
        self.syncParameters()
        wx.PostEvent(self.stf, RefreshGraphEvent())


class DrawOptions():
    defaultKeylist = [["axesType", "Linear-Linear"]]

    def __init__(self):
        self.axesType = "Linear-Linear"

    def setAxesType(self, tl):
        self.axesType = tl

    def updateAxes(self, fig, axes):
        [y, x] = self.axesType.split("-")
        if y == "Log":
            axes.set_yscale('log')
        elif y == "Linear":
            axes.set_yscale('linear')

        if x == "Log":
            axes.set_xscale('log')
        elif x == "Linear":
            axes.set_xscale('linear')

    def updateAxes3(self, fig, axes, axesType):
        [y, x] = axesType.split("-")
        if y == "Log":
            axes.set_yscale('log')
        elif y == "Linear":
            axes.set_yscale('linear')

        if x == "Log":
            axes.set_xscale('log')
        elif x == "Linear":
            axes.set_xscale('linear')

    def makeControlPanel(self, parentWindow):  # Default options
        return DefaultControlPanel(parentWindow)

    def drawTime(self, fig, axes, extra=""):
        """fp = dict(size=10, alpha=0.5)
        _at = AnchoredText("Time: "+str(self.simTime), loc=7, prop=fp)
        _at.patch.set_boxstyle("round,pad=0.,rounding_size=0.2")
        axes.add_artist(_at)"""
        if self.simTime != None:
            axes.annotate("Time: " + str(self.simTime) + extra, xy=(0.0, 1.05), xycoords='axes fraction')

    def scaleYAxis(self, fig, axes, ydata, factor):
        if self._PV.has_key("ymin") and self._PV.has_key("ymax"):
            ymin = self._PV["ymin"]
            ymax = self._PV["ymax"]
        else:
            ymin, ymax = axes.get_ylim()
        dmin = np.min(ydata)
        dmax = np.max(ydata)
        if dmin < ymin:
            ymin = ymin - factor * (ymax - ymin) / 2.0
        if dmax > ymax:
            ymax = ymax + factor * (ymax - ymin) / 2.0
        axes.set_ylim([ymin, ymax])
        self._PV["ymin"] = ymin
        self._PV["ymax"] = ymax


class DrawTrajectory(DrawOptions, KeyList):
    def __init__(self, ydata):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        self.ydata = ydata
        self.plottype = "DRAWTRAJ"

    def drawPlot(self, fig, axes):
        self.syncParameters()
        self.setAxesType(self.PaxesType)
        self.updateAxes(fig, axes)
        # axes.set_xlim(0,len(self.ydata)-1)
        num = len(self.ydata) - 1
        xlim = self.ydata[0]
        xax = np.linspace(-xlim, xlim, num)
        axes.plot(xax, self.ydata[1:])
        self.drawTime(fig, axes)

class DrawMultipleTrajectories(DrawOptions):
    def __init__(self, name, data, itt, t0, dt, defaultplot, graphoptions=None, title=None):
        DrawOptions.__init__(self)
        self.data = data
        self.itt = itt
        self.plottype = name
        self.title = title
        self.t0 = t0
        self.dt = dt
        d1,d2,d3 = np.shape(data)
        # Check if default plot is not out of bounds, d2
        assert( defaultplot >= 0 and defaultplot < d2)
        self.whichplot = defaultplot

    def drawPlot(self, fig, axes):
        numpoints, ts, plotnum = np.shape(self.data)
        if self.itt < 1: #Must include at least 1 data point to draw
            return
        xax = np.linspace(self.t0, self.t0 + (self.itt-1)*self.dt, self.itt)
        for i in range(plotnum):
            axes.plot(xax, self.data[0:self.itt,self.whichplot,i])
        if self.title is not None:
            axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")
        else:
            axes.set_title(self.plottype, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")
        axes.set_xlabel("Time")
        axes.set_ylabel("Position")
        self.drawTime(fig, axes)



class DrawElectronDensity(DrawOptions, KeyList):
    def __init__(self, ydata, nx):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        self.ydata = ydata
        self.plottype = "DRAWDENSE"
        self.nx = nx

    def drawPlot(self, fig, axes):
        self.syncParameters()
        self.setAxesType(self.PaxesType)
        self.updateAxes(fig, axes)
        xax = np.linspace(0, self.nx, self.nx)
        print self.nx
        axes.set_xlim(0, self.nx)
        axes.plot(xax[0:self.nx - 1], self.ydata[0:self.nx - 1])
        # self.scaleYAxis(fig, axes, self.ydata[0:self.nx-1], 2.0)
        self.drawTime(fig, axes)

        # well defined axis here


"""
This plot draws a simple line plot
"""
class DrawSimple(DrawOptions, KeyList):
    def __init__(self, name, xdata, ydata, text, graphoptions=None, title=None):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        self.graphoptions = graphoptions
        if type(name) != type([]):
            self.nl = [name, ""]
        else:
            self.nl = name
            if len(self.nl) != len(xdata) + 1:
                sys.stderr.write(
                    "You the 0th entry in name must be the overlay name.  On other words name is 1 element larger than xdata!")
                exit(0)
        if type(ydata) != type([]):
            self.ydata = [ydata]
        else:
            self.ydata = ydata
        if type(xdata) != type([]):
            self.xdata = [xdata]
        else:
            self.xdata = xdata
        self.text = text
        self.plottype = self.nl[0]
        self.m_Title = title

    def drawPlot(self, fig, axes):
        print(self._PV)
        if 'ylimits' not in self._PV:    #Set default limits on y axis
            self._PV['ylimits'] = (0, 0)
        self.syncParameters()
        self.setAxesType(self.PaxesType)
        for i, ydata in enumerate(self.ydata):
            axes.plot(self.xdata[i], ydata, "-", label=self.nl[i + 1])
            axes.set_xlim(self.xdata[i][0], self.xdata[i][-1])
            #Set limits on Y data to only move on powers of 2
            (oldbottom, oldtop) = self._PV['ylimits']
            (bottom, top) = axisPowerOfTwo(ydata)
            ylim = (min(bottom, oldbottom), max(top, oldtop))
            axes.set_ylim(ylim)
            self._PV['ylimits'] = ylim
        self.updateAxes(fig, axes)
        leg = axes.legend()
        if leg is not None:
            leg.get_frame().set_alpha(0.2)
        axes.annotate(self.text, xy=(0.0, 1.05), xycoords='axes fraction')
        if self.m_Title is None:
            self.m_Title = self.plottype
        axes.set_title(self.m_Title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")
        # self.drawTime(fig, axes, self.text )
        # well defined axis here

    def makeControlPanel(self, parentWindow):  # Default options
        return ExperimentalControlPanel(self, parentWindow)


class DrawScaler(DrawOptions, KeyList):
    def __init__(self, name, ydata, nx, time):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        self.ydata = ydata
        self.plottype = name
        self.nx = nx

    def drawPlot(self, fig, axes):
        self.syncParameters()
        self.setAxesType(self.PaxesType)
        self.updateAxes(fig, axes)
        xax = np.linspace(0, self.nx, self.nx)
        axes.set_xlim(0, self.nx)
        axes.plot(xax[0:self.nx - 1], self.ydata[0:self.nx - 1])
        self.drawTime(fig, axes)

        # well defined axis here


class DrawVelocity(DrawOptions, KeyList):
    def __init__(self, ydata, labels, simtime=None, fvm=None, title=None):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        # self.ydata = [ydata]
        self.labels = labels
        w, h = np.shape(ydata)
        self.ydata = []
        for i in range(h):
            self.ydata.append(ydata[:, i])
        if len(labels) != len(self.ydata):
            sys.stderr.write("Labels must be the same length as the columns in ydata!\n")
            exit(0)
        self.plottype = "DRAWVELOCITY"
        self.simTime = simtime
        vmax = self.ydata[0][0]
        vmin = -vmax
        self.xax = NP.linspace(vmin, vmax, NP.size(self.ydata[0][1:]))
        self.fvm = fvm
        self.title = title

    def drawPlot(self, fig, axes):
        try:
            self.syncParameters()
            self.setAxesType(self.PaxesType)
        except AttributeError:
            self.PaxesType = "Linear-Linear"
        self.updateAxes(fig, axes)
        pidx = 0
        for i, ydata in enumerate(self.ydata):
            axes.plot(self.xax, ydata[1:], '-x', label=self.labels[i])
        #Display x axis label with moment info
        extText = ""
        dimlabels = ["x","y","z"]
        if self.fvm is not None:
            for i,dim in enumerate(self.fvm): # Iterate through dimensions
                try:
                    extText += dimlabels[i]+": VD=" + str(dim[0])
                    extText += "   VTH=" + str(dim[1]) + "\n"
                except IndexError:
                    sys.stderr.write("fv must be at least an n by 2 matrix")
        axes.set_xlabel(r"Velocity"
           "\n" + extText)
        self.drawTime(fig, axes, "")
        leg = axes.legend()
        if leg is not None:
            leg.get_frame().set_alpha(0.2)
        if self.title is None:
            self.title = self.plottype
        axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")
        fig.tight_layout(rect=[0,0,1,0.95])

class DrawPotential(DrawOptions, KeyList):
    def __init__(self, ydata):
        DrawOptions.__init__(self)
        self.setupKeylist(DrawOptions.defaultKeylist)
        self.ydata = ydata
        self.plottype = "DRAWPOT"

    def drawPlot(self, fig, axes):
        self.syncParameters()
        self.setAxesType(self.PaxesType)
        self.updateAxes(fig, axes)
        axes.set_xlim(0, len(self.ydata) - 1)
        axes.plot(self.ydata)
        self.drawTime(fig, axes)
        axes.set_title("Potential vs X", horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")

        # well defined axis here


class DrawEnergyControlPanel(BaseControlPanel):
    def __init__(self, parent, pv, labels):
        BaseControlPanel.__init__(self, parent)
        self._PV = pv
        self.labels = labels
        vsizer1 = self.SetupControls()
        vsizer1.SetSizeHints(self)
        self.SetSizer(vsizer1)
        self.SetAutoLayout(1)
        self.Show()

        # The following methods should be overriden if inherited

    def _buildOptionsList(self):
        items = self._PV["EnergyTypes"].iteritems()
        return [i[0] for i in items if not i[1]["on"]]


    def SetupControls(self):
        vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
        hs1 = wx.BoxSizer(orient=wx.VERTICAL)

        # To offset energy or not
        self.offset_choices = ["Offset Energies", "Magnitude"]
        cb = wx.Choice(self, -1, choices=self.offset_choices, style=wx.CB_READONLY)
        cb.SetStringSelection(self.offset_choices[0] if self._PV["EnergyOffset"] else self.offset_choices[1])
        hs1.Add(cb, flag=wx.ALL | wx.EXPAND)
        cb.Bind(wx.EVT_CHOICE, self.OnSelectOffset)


        #Select axis type
        self.axesTypeList = wx.Choice(self, -1, choices=["Linear-Linear", "Log-Linear", "Linear-Log", "Log-Log"],
                                        style=wx.CB_READONLY)
        self.axesTypeList.SetStringSelection(self._PV["Axis-Type"])
        hs1.Add(self.axesTypeList, flag=wx.ALL | wx.EXPAND)

        # List to store the dynamically generate combo boxes
        self.energyTypeList = []
        # Generate combo boxes
        augmentedLabels = ["Off"] + self.labels #Create list of possible options
        for item in self.labels:
            item_data = self._PV["EnergyTypes"][item]
            is_on = item_data["on"]
            etl = wx.Choice(self, -1, choices=augmentedLabels, style=wx.CB_READONLY)
            selection_value = item if is_on else "Off"
            etl.SetStringSelection(selection_value)
            self.energyTypeList.append([etl, selection_value])  # Append tuple with the selected value of the box
            etl.Bind(wx.EVT_CHOICE, self.OnSelectWK)
            hs1.Add(etl, flag=wx.ALL | wx.EXPAND)
        vsizer1.Add(hs1)
        self.axesTypeList.Bind(wx.EVT_CHOICE, self.OnSelect)
        return vsizer1

    def OnSelect(self, event):
        self._PV["Axis-Type"] = event.GetString()
        wx.PostEvent(self.stf, RefreshGraphEvent())


    def OnSelectWK(self, event):
        caller = event.GetEventObject()
        callerid = -1
        item_data = self._PV["EnergyTypes"]
        for i, etuple in enumerate(self.energyTypeList):
            e,last_value = etuple
            if e.GetId() == caller.GetId():
                callerid = i  # Off has position 0
                graph_name = event.GetString()
                if graph_name == "Off":
                    if last_value != "Off": #Not already Off
                        item_data[last_value]["on"] = False # Set shared state to Off
                        etuple[1] = "Off"    # Update menu state
                else:
                    if item_data[graph_name]["on"]: # Already On
                        e.SetStringSelection(last_value)
                    else: # New selection is not on
                        if last_value != "Off": # Turn Off previous plot
                            item_data[last_value]["on"] = False
                        item_data[graph_name]["on"] = True
                        etuple[1] = graph_name
                wx.PostEvent(self.stf, RefreshGraphEvent())

    def OnSelectOffset(self, event):
        selection = event.GetString()
        if selection == self.offset_choices[0]:
            self._PV["EnergyOffset"] = True
        else:
            self._PV["EnergyOffset"] = False
        wx.PostEvent(self.stf, RefreshGraphEvent())

#Finds the nearest power of 2, greater than or equal to x
def _upperpowerof2(x):
    c = 0
    ox = x
    lessThanOne = False
    if x == 0.0:
        return 0
    if abs(x) < 1.0:
        x = 1.0/x
        c = -1
        lessThanOne = True
    x = int(abs(x))
    while(x) > 0:
        c += 1
        x = x >> 1
    if lessThanOne:
        result = 0.5**c
    else:
        result = 2.0**c
    assert(result >= ox)
    return result

# Finds the nearest power of 2, less than or equal to x
def _lowerpowerof2(x):
    c = -1
    ox = x
    lessThanOne = False
    if x == 0.0:
        return 0
    if abs(x) < 1.0:
        x = 1.0/x
        c = 0
        lessThanOne = True
    x = int(abs(x))
    while(x) > 0:
        c += 1
        x = x >> 1
    if lessThanOne:
        result = 0.5**c
    else:
        result = 2.0**c
    assert(result <= ox)
    return result

def upperpowerof2(x):
    if x < 0:
        return -1*_lowerpowerof2(x)
    else:
        return _upperpowerof2(x)

def lowerpowerof2(x):
    if x < 0:
        return -1*_upperpowerof2(x)
    else:
        return _lowerpowerof2(x)


def axisPowerOfTwo(data):
    if len(data) == 0:
        return (None,None)
    minv = np.amin(data)
    maxv = np.amax(data)
    try:
        rv = (lowerpowerof2(minv), upperpowerof2(maxv))
    except OverflowError:
        print (minv, maxv)
        rv = (-1,1)
    return rv


class DrawEnergy(DrawOptions):
    def __init__(self, data, itw, labels, timeindex=-1, title=None):
        self.edata = data
        self.itw = itw
        self.plottype = "ENERGY"
        self.timeindex = timeindex
        self.labels = labels
        if len(labels) != len(data[0]):
            print "Length of lables(", len(labels), ") is not equal to length of energy data(", len(data[0]), ")"
            exit(0)
        self.title = title

    def drawPlot(self, fig, axes):
        print(self._PV)
        # Create the data model for communication with options,
        # if it does not exist in _PV
        if "EnergyTypes" not in self._PV:
            self._PV["EnergyTypes"] = {x:{"on":False, "index":i} for i,x in enumerate(self.labels)}
            self._PV["EnergyTypes"][self.labels[0]]["on"] = True
        if "EnergyOffset" not in self._PV:
            self._PV["EnergyOffset"] = True
        # Check to see if Axis-Type selector exists.  Default to Linear scale
        if "Axis-Type" not in self._PV:
            self._PV["Axis-Type"] = "Linear-Linear"
        # Use set axis types
        self.updateAxes3(fig, axes, self._PV["Axis-Type"])
        # Draw the plots that are on
        ax_top = None;
        ax_bot = None;
        #Plot all enabled plots
        for item in self._PV["EnergyTypes"].iteritems():
            state = item[1]
            if state["on"]: #Plot if state is on
                data_index = state["index"]
                xdata = self.itw[0:self.timeindex]
                ydata = self.edata[0:self.timeindex, data_index]
                # Offset energy from original value?
                yoffset = 0.0
                if len(ydata) > 0 and self._PV["EnergyOffset"]:
                	yoffset = ydata[0]
                axes.plot(xdata, ydata - yoffset, "-", label=self.labels[data_index])
                # Calculate y-axis limits
                [bottom, top] = axisPowerOfTwo(ydata - yoffset)
                ax_top = max(ax_top, top)
                ax_bot = min(ax_bot, bottom) if ax_bot is not None else bottom
        # Set the limits if valid values are specified
        if ax_top is not None and ax_bot is not None:
            axes.set_ylim([ax_bot, ax_top])
        # Draw titles and time
        self.drawTime(fig, axes)
        leg = axes.legend()
        if leg is not None:
            leg.get_frame().set_alpha(0.2)
        axes.set_xlabel("Time")
        axes.set_ylabel("Energy")
        if self.timeindex == 0:
            axes.set_xlim([0,1])
        else:
            axes.set_xlim([0, np.amax(self.itw[0:self.timeindex])])
        #Set title
        if self.title is None:
            self.title = self.plottype
        axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")

    def makeControlPanel(self, parentWindow):  # Default options
        return DrawEnergyControlPanel(parentWindow, self._PV, self.labels)


class DrawPhase(DrawOptions):
    def __init__(self, data, title=None, twophase=None, plotlabels=["Position", "Velocity"]):
        # DrawOptions.__init__(self)
        self.vel = data[1]
        self.pos = data[0]
        self.lbl = None
        if twophase is not None:
            self.lbl = data[twophase] #particle labels
        self.tp = twophase #Draw particles past twophasedivide a different color
        self.plottype = "DRAWPHASE"
        self.title = title
        self.plabels = plotlabels

    def drawPlot(self, fig, axes):
        la = len(self.pos)
        # self.updateAxes(fig,axes)
        axes.set_xlim(NP.amin(self.pos), NP.amax(self.pos))
        if self.tp is None or self.tp < 0 or self.tp > la:
            axes.plot(self.pos, self.vel, ',b')
        else: #Color beam particles red
            #This is not efficient.
            npxb = np.sum(self.lbl < 0)
            # Data for blue points
            bluesp = 0 # blue stack pointer
            bluevel = np.empty(la - npxb)
            bluepos = np.empty(la - npxb)
            # Data for red points
            redsp = 0 # red stack pointer
            redvel = np.empty(npxb)
            redpos = np.empty(npxb)
            # Categorize data in to red or blue
            for i in range(la):
                if self.lbl[i] < 0: # Marked particles are red
                    redvel[redsp] = self.vel[i]
                    redpos[redsp] = self.pos[i]
                    redsp += 1
                else: # Otherwise they are blue
                    bluevel[bluesp] = self.vel[i]
                    bluepos[bluesp] = self.pos[i]
                    bluesp += 1
            # Plot the two different sets of particles
            axes.plot(redpos, redvel, color=(1,0,0,0.5), marker=',', linestyle='None')
            axes.plot(bluepos, bluevel, color=(0,0,1,0.5), marker=',', linestyle='None')
        axes.set_xlabel(self.plabels[0])
        axes.set_ylabel(self.plabels[1])
        fig.tight_layout()


        self.drawTime(fig, axes)
        #Set title
        if self.title is None:
            self.title = self.plottype
        axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")


class DrawFastPhase(DrawOptions):
    def __init__(self, x, y, h):
        # DrawOptions.__init__(self)
        self.vel = y
        self.pos = x
        self.height = h
        self.plottype = "DRAWFASTPHASE"

    def drawPlot(self, fig, axes):
        x = self.pos
        y = self.vel
        # axes.pcolormesh(self.pos, self.vel, self.height)

        H = np.rot90(self.height)
        H = np.flipud(H)

        # reset figure
        fig.delaxes(axes)
        fig.clf()
        axes = fig.add_subplot(111)

        mim = axes.pcolor(x, y, H, cmap=cm.hot)
        fig.colorbar(mim)
        # set the limits of the plot to the limits of the data
        axes.axis([x.min(), x.max(), y.min(), y.max()])

        # axes.imshow(self.height, interpolation='nearest', origin='low',extent=[self.pos[0], self.pos[-1], self.vel[0], self.vel[-1]])
        self.drawTime(fig, axes)

        return axes  # must return axes if you made new axes


class PhiControlPanel(BaseControlPanel):
    defaultKeylist = [["OmegaMax", 2.0], ["FrameMax", 2 ** 8 + 1], ["FrameBound", 2 ** 8 + 1], ["LowerBound", -1]]

    def __init__(self, parent, dt, PV):  # remember to delete self at some point
        self.dt = dt
        self._PV = PV
        BaseControlPanel.__init__(self, parent, PhiControlPanel.defaultKeylist)
        vsizer1 = self.SetupControls()
        vsizer1.SetSizeHints(self)
        self.SetSizer(vsizer1)
        self.Show()

    def SetupControls(self):
        vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
        hs1 = wx.BoxSizer(orient=wx.HORIZONTAL)
        hs2 = wx.BoxSizer(orient=wx.HORIZONTAL)
        hs3 = wx.BoxSizer(orient=wx.HORIZONTAL)
        # hs4 = wx.BoxSizer(orient=wx.HORIZONTAL)

        # Maximum Omega Slider
        self.omegaMinVal = wx.StaticText(self.panel, label="", pos=(20, 90))
        txt = wx.StaticText(self.panel, label='Maximum Omega', pos=(20, 90))
        sld = wx.Slider(self.panel, value=self.POmegaMax, minValue=0.5, maxValue=50, pos=(20, 20), size=(250, -1),
                        style=wx.SL_HORIZONTAL)
        hs1.Add(txt)
        hs1.Add(sld)
        hs1.Add(self.omegaMinVal)
        sld.Bind(wx.EVT_SCROLL, self.OnMaxOmegaScroll)
        sld.Bind(wx.EVT_SCROLL_CHANGED, self.OnSliderRelease)

        # Frame Size
        self.maxFrameVal = wx.StaticText(self.panel, label="", pos=(20, 90))
        txt = wx.StaticText(self.panel, label='Frame Size', pos=(20, 90))
        sld = wx.Slider(self.panel, value=self.PFrameMax, minValue=2, maxValue=self.PFrameBound, pos=(20, 20),
                        size=(250, -1), style=wx.SL_HORIZONTAL)
        hs2.Add(txt)
        hs2.Add(sld)
        hs2.Add(self.maxFrameVal)
        sld.Bind(wx.EVT_SCROLL, self.OnMaxFrameScroll)
        sld.Bind(wx.EVT_SCROLL_CHANGED, self.OnSliderRelease)

        self.timeDir = wx.Choice(self.panel, -1, choices=["Positive Omega", "Negative Omega"], style=wx.CB_READONLY)
        if self._PV["DISP_OMD"] == 1.0:
            self.timeDir.SetStringSelection("Positive Omega")
        else:
            self.timeDir.SetStringSelection("Negative Omega")
        hs3.Add(self.timeDir)
        self.timeDir.Bind(wx.EVT_CHOICE, self.OnOmegaSelect)

        # Lower Bound
        """self.lbVal = wx.StaticText(self.panel, label="", pos=(20, 90))
        txt = wx.StaticText(self.panel, label='Frame Start', pos=(20, 90))
        sld = wx.Slider(self.panel, value=self.PLowerBound, minValue=-1, maxValue=1000, pos=(20, 20), size=(250, -1), style=wx.SL_HORIZONTAL)
        hs3.Add(txt)
        hs3.Add(sld)
        hs3.Add(self.lbVal)
        sld.Bind(wx.EVT_SCROLL, self.OnLBScroll)
        sld.Bind(wx.EVT_SCROLL_CHANGED, self.OnSliderRelease)"""

        vsizer1.Add(hs1)
        vsizer1.Add(hs2)
        vsizer1.Add(hs3)
        # vsizer1.Add(hs4)

        self.setOmegaLabel(self.POmegaMax)
        self.setFrameLabel(self.PFrameMax)

        return vsizer1

    def setOmegaLabel(self, val):
        self.POmegaMax = val
        self.omegaMinVal.SetLabel(str(val))

    def OnMaxOmegaScroll(self, event):
        obj = event.GetEventObject()
        val = obj.GetValue()
        self.setOmegaLabel(val)
        self.setParameters()

    def setFrameLabel(self, val):
        if val == self.PFrameBound:
            dlabel = "All"
        else:
            dlabel = val * self.dt
        self.maxFrameVal.SetLabel(str(dlabel))
        self.PFrameMax = val

    def OnMaxFrameScroll(self, event):
        obj = event.GetEventObject()
        val = obj.GetValue()
        self.setFrameLabel(val)
        self.setParameters()

    def OnLBScroll(self, event):
        obj = event.GetEventObject()
        val = obj.GetValue()
        self.PLowerBound = val
        if val == -1:
            self.lbVal.SetLabel("End")
        else:
            self.lbVal.SetLabel(str(val / 1000.0))
        self.setParameters()

    def OnSliderRelease(self, event):
        self.broadcastRedraw()

    def OnOmegaSelect(self, event):
        if event.GetString() == "Positive Omega":
            self._PV["DISP_OMD"] = 1.0
        else:
            self._PV["DISP_OMD"] = -1.0
        print self._PV["DISP_OMD"]
        wx.PostEvent(self.stf, RefreshGraphEvent())


class DrawPhi(KeyList):
    def __init__(self, data, dt, omn=100, title=None):
        self.dt = dt
        self.phikw_time = data[0]
        self.phikw = data[1]
        self.phikw_dta = data[2]
        self.plottype = "DRAWPHI"

        self.setupKeylist(PhiControlPanel.defaultKeylist)  # This specifies the defaults
        self.omegaN = omn
        self.title = title

    def drawPlot(self, fig, axes):
        self.syncParameters()  # This syncronized the parameters for the plot
        data = np.array(self.phikw)
        (t, k) = np.shape(data)
        fres = np.zeros((self.omegaN, k))
        # Find frame bounds
        lb = 0
        ub = np.size(self.phikw_time)

        if self.PFrameMax != self.PFrameBound:  # A frame is specified
            lb = ub - self.PFrameMax
            if lb < 0:
                return

        if not self._PV.has_key("DISP_OMD"):  # Check if I should do positive or negative omega
            self._PV["DISP_OMD"] = 1.0

        if type(self.phikw_time) != type(np.ndarray):
            self.phikw_time = np.array(self.phikw_time)

        for i in range(k):
            res, wax = myFft(self.dt, self.phikw_time[lb:ub] * self._PV["DISP_OMD"], data[lb:ub, i], self.POmegaMax,
                             self.omegaN)
            fres[:, i] = res

            # reset figure
        fig.delaxes(axes)
        fig.clf()
        axes = fig.add_subplot(111)

        # pkw, wax = ftPhikw(self.phikw_time, self.phikw, 0.2, 2.0, 10)
        mim = axes.imshow(fres, cmap=cm.rainbow_r, interpolation='none', aspect='auto',
                          extent=(0, k, 0, self.POmegaMax), norm=LogNorm(), origin='lower')
        fig.colorbar(mim)
        axes.set_xlabel(r"$k$")
        axes.set_ylabel(r"$\omega$")
        axes.annotate(
            "Time Slice: " + str(self.phikw_time[lb] * self.dt) + " to " + str(self.phikw_time[ub - 1] * self.dt),
            xy=(0.0, 1.05), xycoords='axes fraction')

        #Set title
        if self.title is None:
            self.title = self.plottype
        axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")
        # If you crearted new axes, return
        return axes

    def makeControlPanel(self, parentWindow):
        return PhiControlPanel(parentWindow, self.dt, self._PV)


class DrawSimpleImage(KeyList):
    def __init__(self, name, data, text, labl=["", ""], extent=None, title=None, ticks_scale=None, norm='Log'):
        self.text = text
        self.plottype = name
        self.img = data
        self.labl = labl
        self.extent = extent
        self.title = title
        self.ticks_scale = ticks_scale
        self.norm = norm

    def drawPlot(self, fig, axes):
        (t, k) = np.shape(self.img)

        # reset figure
        fig.delaxes(axes)
        fig.clf()
        axes = fig.add_subplot(111)
        if self.norm=='Log':
            normalization = LogNorm()
        else:
            normalization = None

        try:
            mim = axes.imshow(np.transpose(self.img), cmap=cm.rainbow, interpolation='none', aspect='auto',
                                  norm=normalization, origin='lower', extent=self.extent)
            fig.colorbar(mim)
        except:
            mim = axes.imshow(np.transpose(self.img), cmap=cm.rainbow, interpolation='none', aspect='auto',
                              origin='lower', extent=self.extent)
            fig.colorbar(mim)

        axes.set_xlabel(self.labl[0])
        axes.set_ylabel(self.labl[1])
        if self.ticks_scale is not None:
            axes.set_xticklabels([str("{0:.2f}".format(self.ticks_scale*x)) for x in axes.get_xticks()])
        axes.annotate(self.text, xy=(0.0, 1.05), xycoords='axes fraction')
        if self.title is None:
            self.title = self.plottype
        axes.set_title(self.title, horizontalalignment='center', verticalalignment='top', transform=axes.transAxes, fontsize="smaller")

        # If you crearted new axes, return
        return axes

    def makeControlPanel(self, parentWindow):
        return None
