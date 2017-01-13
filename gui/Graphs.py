import copy
import wx
import collections
import functools
import operator
import wx.stc as stc
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import LogNorm
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.backends.backend_wx import NavigationToolbar2Wx
from mpl_toolkits.axes_grid.anchored_artists import AnchoredText

from Events import *
from lib import *



class KeyList:
	#set the location of the dictionary
	def setDict(self,curdict):
		self._dc = curdict

	def setupKeylist(self, keylist):
		self.keyList = copy.deepcopy(keylist)

	#recieve external parameters from panel
	def setParams(self,val):
		self.setDict(val)

	#delete old keys, then load parameters from dictionary
	def updateKeylist(self, keylist):
		#first remove old keys from local context.  Cleanliness reasons
		for key in self.keyList:
			try:
				delattr(self,"P"+key[0])
			except AttributeError:
				True
		self.keyList = keylist #set to new keylist
		self.getParameterValues()
		self.setParameters()

	def getParameterValues(self):
		dc = self._dc
		for key in self.keyList:
			if dc.has_key(key[0]): #set to value in dictionary
				setattr(self,"P"+key[0], dc[key[0]] )
			else: #set default value
				setattr(self,"P"+key[0],key[1] )

	#Load parameters in to object attributes
	def setParameters(self):
		dc = self._dc
		for key in self.keyList:
			dc[key[0]] = getattr(self,"P"+key[0])

	#ensures local variables and parameters match
	def syncParameters(self):
		self.getParameterValues()
		self.setParameters()


#Subclass to make new control panels
class BaseControlPanel(wx.Frame, KeyList):
	def __init__(self, parent, keylist = [ ["axesType","Linear-Linear"] ]):  #remember to delete self at some point
		"""Create the MainFrame."""
		wx.Frame.__init__(self, parent, style=wx.DEFAULT_FRAME_STYLE|wx.FRAME_FLOAT_ON_PARENT)
		self.keyList =  keylist #parameter name in dictionary + default value
		self.stf = parent
		self.setDict(self.stf.arbGraphParameters)
		self.SetPosition((200,200))
		self.panel = wx.Panel(self, -1, wx.DefaultPosition, wx.DefaultSize)
		self.updateKeylist(keylist)
		self.syncParameters()
		self.Bind(wx.EVT_CLOSE, self.OnQuit)

	def OnQuit(self, event):
		self.setParameters()
		wx.PostEvent(self.stf, CloseOptionsEvent() )

	def broadcastRedraw(self):
		wx.PostEvent(self.stf, RefreshGraphEvent() )

class DefaultControlPanel(BaseControlPanel):
	def __init__(self, parent):
		BaseControlPanel.__init__(self,parent)
		vsizer1 = self.SetupControls()
		self.SetSizer(vsizer1)
		self.SetAutoLayout(1)
		vsizer1.Fit(self)
		self.Show()

	#The following methods should be overriden if inherited
	def SetupControls(self):
		vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
		hs1 = wx.BoxSizer(orient=wx.HORIZONTAL)

		self.axesTypeList = wx.ComboBox(self,-1, choices = ["Linear-Linear","Log-Linear","Linear-Log","Log-Log"],style=wx.CB_READONLY)
		self.axesTypeList.SetStringSelection(self.PaxesType)
		hs1.Add(item=self.axesTypeList)

		vsizer1.Add(hs1)

		self.axesTypeList.Bind(wx.EVT_COMBOBOX, self.OnSelect)

		return vsizer1

	def OnSelect(self,event):
		self.stf.arbGraphParameters["axesType"] = event.GetString()
		self.syncParameters()
		wx.PostEvent(self.stf, RefreshGraphEvent() )


class DrawOptions():
	defaultKeylist = [["axesType","Linear-Linear"]]
	def __init__(self):
		self.axesType = "Linear-Linear"

	def setAxesType(self, tl):
		self.axesType = tl

	def updateAxes(self, fig, axes):
		[y,x] = self.axesType.split("-")
		if y == "Log":
			axes.set_yscale('log')
		elif y == "Linear":
			axes.set_yscale('linear')

		if x == "Log":
			axes.set_xscale('log')
		elif x == "Linear":
			axes.set_xscale('linear')

	def updateAxes3(self, fig, axes, axesType):
		[y,x] = axesType.split("-")
		if y == "Log":
			axes.set_yscale('log')
		elif y == "Linear":
			axes.set_yscale('linear')

		if x == "Log":
			axes.set_xscale('log')
		elif x == "Linear":
			axes.set_xscale('linear')

	def makeControlPanel(self, parentWindow):  #Default options
		return DefaultControlPanel(parentWindow)

	def drawTime(self,fig,axes,extra=""):
		"""fp = dict(size=10, alpha=0.5)
		_at = AnchoredText("Time: "+str(self.simTime), loc=7, prop=fp)
		_at.patch.set_boxstyle("round,pad=0.,rounding_size=0.2")
		axes.add_artist(_at)"""
		if self.simTime != None:
			axes.annotate("Time: "+str(self.simTime)+extra, xy=(0.0, 1.05), xycoords='axes fraction')

	def scaleYAxis(self,fig,axes, ydata, factor):
		if self._PV.has_key("ymin") and self._PV.has_key("ymax"):
			ymin = self._PV["ymin"]
			ymax = self._PV["ymax"]
		else:
			ymin, ymax = axes.get_ylim()
		dmin = np.min(ydata)
		dmax = np.max(ydata)
		if dmin < ymin:
			ymin = ymin - factor*(ymax-ymin)/2.0
		if dmax > ymax:
			ymax = ymax + factor*(ymax-ymin)/2.0
		axes.set_ylim([ymin,ymax])
		self._PV["ymin"] = ymin
		self._PV["ymax"] = ymax


class DrawTrajectory(DrawOptions, KeyList):
	def __init__(self,ydata):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		self.ydata = ydata
		self.plottype = "DRAWTRAJ"

	def drawPlot(self, fig, axes):
		self.syncParameters()
		self.setAxesType(self.PaxesType)
		self.updateAxes(fig,axes)
		#axes.set_xlim(0,len(self.ydata)-1)
		num = len(self.ydata) - 1
		xlim = self.ydata[0]
		xax = np.linspace(-xlim,xlim,num)
		axes.plot(xax, self.ydata[1:])
		self.drawTime(fig, axes)
		#well defined axis here

class DrawElectronDensity(DrawOptions, KeyList):
	def __init__(self,ydata, nx):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		self.ydata = ydata
		self.plottype = "DRAWDENSE"
		self.nx = nx

	def drawPlot(self, fig, axes):
		self.syncParameters()
		self.setAxesType(self.PaxesType)
		self.updateAxes(fig,axes)
		xax = np.linspace(0,self.nx, self.nx)
		print self.nx
		axes.set_xlim(0,self.nx)
		axes.plot(xax[0:self.nx-1], self.ydata[0:self.nx-1] )
		#self.scaleYAxis(fig, axes, self.ydata[0:self.nx-1], 2.0)
		self.drawTime(fig, axes)
		#well defined axis here

class DrawSimple(DrawOptions, KeyList):
	def __init__(self, name, xdata, ydata, text, graphoptions=None):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		self.graphoptions = graphoptions
		if type(name) != type([]):
			self.nl = [name,""]
		else:
			self.nl = name
			if len(self.nl) != len(xdata) + 1:
				sys.stderr.write("You the 0th entry in name must be the overlay name.  On other words name is 1 element larger than xdata!")
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

	def drawPlot(self, fig, axes):
		self.syncParameters()
		self.setAxesType(self.PaxesType)
		for i, ydata in enumerate(self.ydata):
			axes.plot(self.xdata[i], ydata,"-", label=self.nl[i+1] )
		self.updateAxes(fig,axes)
		leg = axes.legend()
		leg.get_frame().set_alpha(0.5)
		axes.annotate(self.text, xy=(0.0, 1.05), xycoords='axes fraction')
		#self.drawTime(fig, axes, self.text )
		#well defined axis here

class DrawScaler(DrawOptions, KeyList):
	def __init__(self, name,ydata, nx, time):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		self.ydata = ydata
		self.plottype = name
		self.nx = nx

	def drawPlot(self, fig, axes):
		self.syncParameters()
		self.setAxesType(self.PaxesType)
		self.updateAxes(fig,axes)
		xax = np.linspace(0,self.nx, self.nx)
		axes.set_xlim(0,self.nx)
		axes.plot(xax[0:self.nx-1], self.ydata[0:self.nx-1] )
		self.drawTime(fig, axes)
		#well defined axis here

class DrawVelocity(DrawOptions, KeyList):
	def __init__(self,ydata,labels,simtime=None,fvm=None):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		#self.ydata = [ydata]
		self.labels = labels
		w,h = np.shape(ydata)
		self.ydata = []
		for i in range(h):
			self.ydata.append(ydata[:,i])
		if len(labels) != len(self.ydata):
			sys.stderr.write("Labels must be the same length as the columns in ydata!\n")
			exit(0)
		self.plottype = "DRAWVELOCITY"
		self.simTime = simtime
		vmax = self.ydata[0][0]
		vmin = -vmax
		self.xax = NP.linspace(vmin,vmax,NP.size(self.ydata[0][1:]))
		self.fvm = [fvm]

	def drawPlot(self, fig, axes):
		try:
			self.syncParameters()
			self.setAxesType(self.PaxesType)
		except AttributeError:
			self.PaxesType = "Linear-Linear"
		self.updateAxes(fig,axes)
		pidx = 0
		for i,ydata in enumerate(self.ydata):
			axes.plot(self.xax, ydata[1:],'-x',label = self.labels[i])
			extText = ""
			if self.fvm != None:
				try:
					extText = "  VTX = " + str(self.fvm[i][1])
				except:
					True#extText = "  VTX = " + str(self.fvm[i][1]  )
		self.drawTime(fig, axes, extText)
		self.scaleYAxis(fig,axes, ydata[1:], 2.0)
		axes.legend()	

class DrawPotential(DrawOptions, KeyList):
	def __init__(self,ydata):
		DrawOptions.__init__(self)
		self.setupKeylist(DrawOptions.defaultKeylist)
		self.ydata = ydata
		self.plottype = "DRAWPOT"

	def drawPlot(self, fig, axes):
		self.syncParameters()
		self.setAxesType(self.PaxesType)
		self.updateAxes(fig,axes)
		axes.set_xlim(0,len(self.ydata)-1)
		axes.plot(self.ydata)
		self.drawTime(fig, axes)
		#well defined axis here

class DrawEnergyControlPanel(BaseControlPanel):
	def __init__(self, parent,pv,labels):
		BaseControlPanel.__init__(self,parent)
		self._PV = pv
		self.labels = labels
		self.labels.insert(0,"Off")
		vsizer1 = self.SetupControls()
		vsizer1.SetSizeHints(self)
		self.SetSizer(vsizer1)
		self.SetAutoLayout(1)
		self.Show()

	#The following methods should be overriden if inherited
	def SetupControls(self):
		vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
		hs1 = wx.BoxSizer(orient=wx.VERTICAL)

		self.axesTypeList = wx.ComboBox(self,-1, choices = ["Linear-Linear","Log-Linear","Linear-Log","Log-Log"],style=wx.CB_READONLY)
		self.axesTypeList.SetStringSelection(self._PV["Axis-Type"] )
		hs1.Add(item=self.axesTypeList)


		self.energyTypeList = []
		for i,label in enumerate(self.labels[1:]):
			etl = wx.ComboBox(self,-1, choices = self.labels, style=wx.CB_READONLY)
			self.energyTypeList.append(etl)
			etl.SetStringSelection(self._PV["Energy_Type"+str(i)])
			etl.Bind(wx.EVT_COMBOBOX, self.OnSelectWK)
			hs1.Add(item=etl, flag=wx.ALL | wx.EXPAND)
		"""self.energyTypeList = wx.ComboBox(self,-1, choices = ["Total","Potential","Kinetic"],style=wx.CB_READONLY)
		self.energyTypeList.SetStringSelection(self._PV["Energy_Type"])
		hs1.Add(item=self.energyTypeList)

		self.energyTypeList2 = wx.ComboBox(self,-1, choices = ["Off","Total","Potential","Kinetic"],style=wx.CB_READONLY)
		self.energyTypeList2.SetStringSelection(self._PV["Energy_Type2"])
		hs1.Add(item=self.energyTypeList2)

		self.energyTypeList3 = wx.ComboBox(self,-1, choices = ["Off", "Total","Potential","Kinetic"],style=wx.CB_READONLY)
		self.energyTypeList3.SetStringSelection(self._PV["Energy_Type3"])
		hs1.Add(item=self.energyTypeList3)"""

		vsizer1.Add(hs1)

		self.axesTypeList.Bind(wx.EVT_COMBOBOX, self.OnSelect)
		"""self.energyTypeList.Bind(wx.EVT_COMBOBOX, self.OnSelectWKE)
		self.energyTypeList2.Bind(wx.EVT_COMBOBOX, self.OnSelectWKE2)
		self.energyTypeList3.Bind(wx.EVT_COMBOBOX, self.OnSelectWKE3)"""

		return vsizer1

	def OnSelect(self,event):
		self._PV["Axis-Type"] = event.GetString()
		wx.PostEvent(self.stf, RefreshGraphEvent() )

	def OnSelectWK(self, event):
		caller = event.GetEventObject()
		callerid = -1
		for i,e in enumerate(self.energyTypeList):
			if e.GetId() == caller.GetId():
				callerid = i #Off has position 0
		if callerid != -1:
			self._PV["Energy_Type"+str(callerid)] = event.GetString()
			wx.PostEvent(self.stf, RefreshGraphEvent() )
			print "Energy_Type"+str(callerid), event.GetString()

	def OnSelectWKE(self, event):
		self._PV["Energy_Type"] = event.GetString()
		wx.PostEvent(self.stf, RefreshGraphEvent() )

	def OnSelectWKE2(self, event):
		self._PV["Energy_Type2"] = event.GetString()
		wx.PostEvent(self.stf, RefreshGraphEvent() )

	def OnSelectWKE3(self, event):
		self._PV["Energy_Type3"] = event.GetString()
		wx.PostEvent(self.stf, RefreshGraphEvent() )

class DrawEnergy(DrawOptions):
	eind = {"Kinetic":1, "Total":3, "Potential":0, "Off":-1} #Selection to index
	def __init__(self,data, itw, labels, timeindex = -1):
		self.edata = data
		self.itw = itw
		self.plottype = "ENERGY"
		self.timeindex = timeindex
		self.labels = labels
		if len(labels) != len(data[0]):
			print "Length of lables(",len(labels), ") is not equal to length of energy data(",len(data[0]),")"
			exit(0)

	def drawPlot(self,fig, axes):
		#Set default values if they do not exist
		#_PV is a dictionary of persistent values, that
		if not self._PV.has_key("Energy_Type0"):
			self._PV["Energy_Type0"] = self.labels[0]
			for i,l in enumerate(self.labels[1:]):
				self._PV["Energy_Type"+str(i+1)] = "Off"
			self._PV["Axis-Type"] = "Linear-Linear"
		#Draw Default plot
		self.updateAxes3(fig,axes,self._PV["Axis-Type"])
		"""try:
			si = self.labels.index(self._PV["Energy_Type0"])
			axes.plot(self.itw[0:self.timeindex], self.edata[0:self.timeindex, si],"x")
		except ValueError: #Off not in list
			True"""

		for i,l in enumerate(self.labels):
			try:
				lbl = self._PV["Energy_Type"+str(i)]
				si = self.labels.index(lbl)
				axes.plot(self.itw[0:self.timeindex], self.edata[0:self.timeindex, si],"x", label=lbl)
			except ValueError: #Off not in list
				True

		#Draw optional overlays
		"""print self._PV["Energy_Type2"] 
		if DrawEnergy.eind[ self._PV["Energy_Type2"] ] != -1:
			si = DrawEnergy.eind[self._PV["Energy_Type2"]]
			axes.plot(self.itw[0:self.timeindex], self.edata[0:self.timeindex, si],"x")
		if DrawEnergy.eind[ self._PV["Energy_Type3"] ] != -1:
			si = DrawEnergy.eind[self._PV["Energy_Type3"]]
			axes.plot(self.itw[0:self.timeindex], self.edata[0:self.timeindex, si],"x")"""

		#self.scaleYAxis(fig,axes, self.edata[0:self.timeindex], 0.1)
		self.drawTime(fig, axes)
		axes.legend()
		axes.set_xlabel("Time")
		axes.set_ylabel("Energy")

	def makeControlPanel(self, parentWindow):  #Default options
		temp = DrawEnergyControlPanel(parentWindow, self._PV, self.labels)
		return temp

class DrawPhase(DrawOptions):
	def __init__(self,data):
		#DrawOptions.__init__(self)
		self.vel = data[1]
		self.pos = data[0]
		self.plottype = "DRAWPHASE"

	def drawPlot(self, fig, axes):
		la = len(self.pos) - 1
		#self.updateAxes(fig,axes)
		axes.set_xlim(NP.amin(self.pos), NP.amax(self.pos) )
		axes.plot(self.pos, self.vel, ',b')
		self.drawTime(fig, axes)

#(self, partd, itt, comp)
class DrawMultiTraj(DrawOptions):
	def __init__(self,partd, itt, comp):
		#DrawOptions.__init__(self)
		self.partd = partd
		self.itt = itt
		self.comp = comp
		self.plottype = "DRAWMULTITRAJ"

	def drawPlot(self, fig, axes):
		x,y,z = np.shape(self.partd)
		if self.comp >= y:
			print "DrawMultiTraj is being asked to draw an out of bounds compent ", self.comp
			return
		for i in range(z):
			axes.plot(self.partd[0:self.itt, self.comp, i],label=str(i) )
		#axes.plot(self.pos, self.vel, ',b')
		#axes.legend()
		self.drawTime(fig, axes)

class DrawFastPhase(DrawOptions):
	def __init__(self,x,y,h):
		#DrawOptions.__init__(self)
		self.vel = y
		self.pos = x
		self.height = h
		self.plottype = "DRAWFASTPHASE"

	def drawPlot(self, fig, axes):
		x = self.pos
		y = self.vel
		#axes.pcolormesh(self.pos, self.vel, self.height)

		H = np.rot90(self.height)
		H = np.flipud(H)

		#reset figure
		fig.delaxes(axes)
		fig.clf()
		axes = fig.add_subplot(111)

		mim = axes.pcolor(x, y, H, cmap=cm.hot)
		fig.colorbar(mim)
		# set the limits of the plot to the limits of the data
		axes.axis([x.min(), x.max(), y.min(), y.max()])

		#axes.imshow(self.height, interpolation='nearest', origin='low',extent=[self.pos[0], self.pos[-1], self.vel[0], self.vel[-1]])
		self.drawTime(fig, axes)

		return axes  #must return axes if you made new axes

class PhiControlPanel(BaseControlPanel):
	defaultKeylist = [ ["OmegaMax",2.0], ["FrameMax",2**8+1], ["FrameBound",2**8+1], ["LowerBound",-1] ]

	def __init__(self, parent, dt, PV):  #remember to delete self at some point
		self.dt = dt
		self._PV = PV
		BaseControlPanel.__init__(self,parent,PhiControlPanel.defaultKeylist)
		vsizer1 = self.SetupControls()
		vsizer1.SetSizeHints(self)
		self.SetSizer(vsizer1)
		self.Show()

	def SetupControls(self):
		vsizer1 = wx.BoxSizer(orient=wx.VERTICAL )
		hs1 = wx.BoxSizer(orient=wx.HORIZONTAL )
		hs2 = wx.BoxSizer(orient=wx.HORIZONTAL )
		hs3 = wx.BoxSizer(orient=wx.HORIZONTAL )
		#hs4 = wx.BoxSizer(orient=wx.HORIZONTAL)

		#Maximum Omega Slider
		self.omegaMinVal = wx.StaticText(self.panel, label="", pos=(20, 90)) 
		txt = wx.StaticText(self.panel, label='Maximum Omega', pos=(20, 90))  
		sld = wx.Slider(self.panel, value=self.POmegaMax, minValue=0.5, maxValue=50, pos=(20, 20), size=(250, -1), style=wx.SL_HORIZONTAL)
		hs1.Add(txt)
		hs1.Add(sld)
		hs1.Add(self.omegaMinVal)
		sld.Bind(wx.EVT_SCROLL, self.OnMaxOmegaScroll)
		sld.Bind(wx.EVT_SCROLL_CHANGED, self.OnSliderRelease)

		#Frame Size
		self.maxFrameVal = wx.StaticText(self.panel, label="", pos=(20, 90)) 
		txt = wx.StaticText(self.panel, label='Frame Size', pos=(20, 90))  
		sld = wx.Slider(self.panel, value=self.PFrameMax, minValue=2, maxValue=self.PFrameBound, pos=(20, 20), size=(250, -1), style=wx.SL_HORIZONTAL)
		hs2.Add(txt)
		hs2.Add(sld)
		hs2.Add(self.maxFrameVal)
		sld.Bind(wx.EVT_SCROLL, self.OnMaxFrameScroll)
		sld.Bind(wx.EVT_SCROLL_CHANGED, self.OnSliderRelease)

		self.timeDir = wx.ComboBox(self.panel,-1, choices = ["Positive Omega","Negative Omega"],style=wx.CB_READONLY)
		if self._PV["DISP_OMD"] == 1.0:
			self.timeDir.SetStringSelection("Positive Omega")
		else:
			self.timeDir.SetStringSelection("Negative Omega")
		hs3.Add(self.timeDir)
		self.timeDir.Bind(wx.EVT_COMBOBOX, self.OnOmegaSelect)

		#Lower Bound
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
		#vsizer1.Add(hs4)

		self.setOmegaLabel(self.POmegaMax)
		self.setFrameLabel(self.PFrameMax)

		return vsizer1

	def setOmegaLabel(self,val):
		self.POmegaMax = val
		self.omegaMinVal.SetLabel(str(val))

	def OnMaxOmegaScroll(self,event):
		obj = event.GetEventObject()
		val = obj.GetValue()
		self.setOmegaLabel(val)
		self.setParameters()
		
	def setFrameLabel(self,val):
		if val == self.PFrameBound:
			dlabel = "All"
		else:
			dlabel = val*self.dt
		self.maxFrameVal.SetLabel(str(dlabel))
		self.PFrameMax = val

	def OnMaxFrameScroll(self,event):
		obj = event.GetEventObject()
		val = obj.GetValue()
		self.setFrameLabel(val)
		self.setParameters()

	def OnLBScroll(self,event):
		obj = event.GetEventObject()
		val = obj.GetValue()
		self.PLowerBound = val
		if val == -1:
			self.lbVal.SetLabel("End")
		else:
			self.lbVal.SetLabel(str(val/1000.0))
		self.setParameters()

	def OnSliderRelease(self,event):
		self.broadcastRedraw()

	def OnOmegaSelect(self,event):
		if event.GetString() == "Positive Omega":
			self._PV["DISP_OMD"] = 1.0
		else:
			self._PV["DISP_OMD"] = -1.0
		print self._PV["DISP_OMD"]
		wx.PostEvent(self.stf, RefreshGraphEvent() )


class DrawPhi( KeyList):
	def __init__(self,data,dt,omn=100):
		self.dt = dt
		self.phikw_time = data[0]
		self.phikw = data[1]
		self.phikw_dta = data[2]
		self.plottype = "DRAWPHI"

		self.setupKeylist(PhiControlPanel.defaultKeylist) #This specifies the defaults
		self.omegaN = omn

	def drawPlot(self, fig, axes):
		self.syncParameters()  #This syncronized the parameters for the plot
		data = np.array(self.phikw)
		(t, k) = np.shape(data)
		fres = np.zeros( (self.omegaN, k) )
		#Find frame bounds
		lb = 0
		ub = np.size(self.phikw_time)

		if self.PFrameMax != self.PFrameBound: #A frame is specified
			lb = ub - self.PFrameMax
			if lb < 0:
				return

		if not self._PV.has_key("DISP_OMD"):  #Check if I should do positive or negative omega
			self._PV["DISP_OMD"] = 1.0

		if type(self.phikw_time) != type(np.ndarray):
			self.phikw_time = np.array(self.phikw_time)

		for i in range(k):
			res, wax = myFft(self.dt, self.phikw_time[lb:ub]*self._PV["DISP_OMD"], data[lb:ub,i], self.POmegaMax, self.omegaN)
			fres[:, i] = res

		#reset figure
		fig.delaxes(axes)
		fig.clf()
		axes = fig.add_subplot(111)

		#pkw, wax = ftPhikw(self.phikw_time, self.phikw, 0.2, 2.0, 10)
		mim = axes.imshow( fres, cmap=cm.rainbow_r, interpolation='none', aspect='auto',extent=(0, k, 0, self.POmegaMax), norm=LogNorm(),origin='lower' )
		fig.colorbar(mim)
		axes.set_xlabel(r"$k$")
		axes.set_ylabel(r"$\omega$")
		axes.annotate("Time Slice: " + str(self.phikw_time[lb]*self.dt) + " to "+str(self.phikw_time[ub-1]*self.dt), xy=(0.0, 1.05), xycoords='axes fraction')

		#If you crearted new axes, return
		return axes

	def makeControlPanel(self, parentWindow):
		return PhiControlPanel(parentWindow, self.dt, self._PV)

class DrawSimpleImage( KeyList):
	def __init__(self,name,data,text,labl=["",""],extent=()):
		self.text = text
		self.plottype = name
		self.img = data
		self.labl = labl
		self.extent = extent

	def drawPlot(self, fig, axes):
		(t, k) = np.shape(self.img)

		#reset figure
		fig.delaxes(axes)
		fig.clf()
		axes = fig.add_subplot(111)

		#pkw, wax = ftPhikw(self.phikw_time, self.phikw, 0.2, 2.0, 10)
		if len(self.extent) == 4:
			mim = axes.imshow( np.transpose(self.img), cmap=cm.rainbow, interpolation='none', aspect='auto', norm=LogNorm(),origin='lower',extent=self.extent )
		else:
			mim = axes.imshow( np.transpose(self.img), cmap=cm.rainbow, interpolation='none', aspect='auto', norm=LogNorm(),origin='lower')
		fig.colorbar(mim)
		axes.set_xlabel(self.labl[0])
		axes.set_ylabel(self.labl[1])
		axes.annotate(self.text, xy=(0.0, 1.05), xycoords='axes fraction')

		#If you crearted new axes, return
		return axes

	def makeControlPanel(self, parentWindow):
		return None