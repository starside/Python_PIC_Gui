import sys
from lib import *
import pyinit1mod as pin
import numpy as NP
import matplotlib.pyplot as plt
import pyfft1mod
import wx
import threading
import matplotlib
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from collections import deque

#TODO:  Use nplots as default window size

in1 = pin.input1d
diag = pin.diag1d
gl = pin.globals
fld = pin.field1d
psm = pin.push1d

QUIET = True

print pyfft1mod.fft1d.pycal()

class C: pass

def error(mess):
	if not QUIET:
		sys.stderr.write(mess)


def cguard(fxu,nx,inorder):
	if len(fxu.shape) == 1:
		fld.idguard1(fxu,nx,inorder)
		error("Using idguard\n")
	elif len(fxu.shape) == 2:
		fld.icguard1(fxu,nx,inorder)
		error("Using icguard\n")
	else:
		error("cguard only takes a 1 or 2d array!\n")
		exit(0)

#This is a way to mimic Fortran generics
def distr(*args):
	argSig = findSig(args)
	#argument signatures
	sigs = {"2f,0i,0i,0f,0f,0i,0i,0i":pin.init1d.idistr1, \
		"2f,0i,0i,0f,0f,0f,0f,0f,0f,0i,0i,0i":pin.init1d.idistrh1,\
		"2f,2f,0i,0f,0i,0i,0i":pin.init1d.ibdistr1,\
		"2f,2f,0i,0f,0i,0i":pin.init1d.ibdistr1,\
		"2f,2f,0i,0f,0f,0i,0i,0i":pin.init1d.irbdistr1,\
		"2f,2f,0i,0f,0f,0i,0i":pin.init1d.irbdistr1}
	if argSig not in sigs:
		sys.stderr.write("Distr does not work with your arguments!\n")
		exit(0)
	error(sigs[argSig].__doc__+"\n") #display which function we called
	sigs[argSig](*args)

#This is a way to mimic Fortran generics
def fft(*args):
	argSig = findSig(args)
	#argument signatures
	sigs = {"1f,0i,1i,1c,0f,0i,0i":pyfft1mod.fft1d.ifft1rx, \
		"1f,0i,1i,1c,0f,0i":pyfft1mod.fft1d.ifft1rx, \
		"1f,0i,1i,1c,0f":pyfft1mod.fft1d.ifft1rx, \
		"2f,0i,1i,1c,0f,0i,0i":pyfft1mod.fft1d.ifft1r2, \
		"2f,0i,1i,1c,0f,0i":pyfft1mod.fft1d.ifft1r2, \
		"2f,0i,1i,1c,0f":pyfft1mod.fft1d.ifft1r2}
	if argSig not in sigs:
		sys.stderr.write("fft does not work with your arguments!\n")
		exit(0)
	error(sigs[argSig].__doc__+"\n") #display which function we called
	sigs[argSig](*args)

#This is a way to mimic Fortran generics
def gtmodes(*args):
	argSig = findSig(args)
	#argument signatures
	sigs = {"1f,1c,0i,0i,0i":fld.igtmodes1, \
		"1f,1c,0i,0i":fld.igtmodes1, \
		"2f,2c,0i,0i,0i":fld.igtvmodes1, \
		"2f,2c,0i,0i":fld.igtvmodes1, \
	}
	if argSig not in sigs:
		sys.stderr.write("gmodes does not work with your arguments!\n")
		exit(0)
	error(sigs[argSig].__doc__+"\n") #display which function we called
	sigs[argSig](*args)

class RunSimulation(threading.Thread):
	def __init__(self,notify_window):
		"""Init Worker Thread Class."""
		threading.Thread.__init__(self)
		self._notify_window = notify_window
		self._want_abort = 0
		self.initFortran()
		self.start()

	def initFortran(self):
		loadNamelist(in1,'input1')			#Read into a fortran module
		loadNamelist(C,'declares')			#read in to python.  C is a class to hold stuff
		#Line 58
		diag.copyint(1,in1.idcode)
		diag.copyint(1,in1.psolve)
		diag.copyint(1,in1.ndim)
		#line 63
		self.cdrun = int(in1.idrun)
		self.dfname = open("output1."+str(int(self.cdrun)),'w')
		#Line 70
		# np = total number of electrons in simulation.
		C.np[0] = in1.npx + in1.npxb
		C.npx1[0] = in1.npx + 1
		C.nx[0] = 2**in1.indx
		C.nxh[0] = C.nx[0]/2
		C.nxe[0] = C.nx[0] + 4
		if in1.inorder == gl.linear:
			C.nxe[0] = C.nx[0] + 2
		elif in1.inorder == gl.cubic:
			C.nxe[0] = C.nx[0] + 6
		# dimension for index and sorting arrays
		C.nx1[0] = C.nx[0] + 1
		# nloop = number of time steps in simulation
		C.nloop[0] = in1.tend/in1.dt + .0001
		# part(1,n) = position x of particle n
		# part(2,n) = velocity vx of particle n
		self.part = NP.empty( (C.idimp,C.np), dtype=fType('real'), order='F' )
		self.qe = NP.empty( C.nxe, dtype=fType('real') )
		self.fxe= NP.empty( C.nxe , dtype=fType('real') )
		# ffc = form factor array for poisson solver
		self.ffc = NP.empty(C.nxh, dtype=fType('complex') )
		# mixup = array of bit reversed addresses for fft
		# sct = sine/cosine table for fft
		self.mixup = NP.empty(C.nxh,dtype=fType('int') )
		self.sct = NP.empty(C.nxh,dtype=fType('complex') )
		# open graphics device
		#C.irc[0] = diag.open_graphs(in1.nplot)
		# initialize timer
		self.tt = diag.wtimer(C.ltime,-1)
		C.time[0] = self.tt
		# initialize constants
		C.itime[0] = C.itime0[0]
		C.ntime[0] = C.itime[0] + C.itime0[0]
		C.qbme[0] = in1.qme
		C.affp[0] = 1.0*C.nx[0]/(1.0*C.np[0])
		# set initial time
		diag.copyint(in1.dt*C.itime0, in1.t0)
		# set default diagnostic file names
		if in1.ntp > 0:
			self.fpname = 'potk1.'#//cdrun
		# energy time history
		if (in1.ntw > 0):
			self.wt = NP.empty( (  (C.nloop-1)/in1.ntw-(C.itime0/in1.ntw)+1 ,4), dtype=fType('real'), order='F' )
			C.itw[0] = 0
		#prepare fft tables
		pyfft1mod.fft1d.ifft1rxinit(self.mixup,self.sct,in1.indx)
		#calculate form factors
		fld.ipois1init(self.ffc, in1.ax, C.affp, C.nx)
		# initialize density profile and velocity distribution
		# background electrons
		if in1.npx > 0:
			distr(self.part,rightType(1),in1.npx,in1.vtx,in1.vx0,in1.npx,C.nx,C.ipbc)
		# beam electrons
		if in1.npxb > 0:
			distr(self.part, C.npx1, in1.npxb, in1.vtdx, in1.vdx, in1.npxb, C.nx, C.ipbc)
		# initialize charge density to background
		C.qi0 = -in1.qme/C.affp
		#! calculate initial electron momentum
		if (in1.ntm > 0):
			psm.initmomt1(self.part, C.np, C.pxe, C.pye, C.pze, in1.ndim)
		# sorting arrays
		if (in1.sortime > 0): 
			self.pt = NP.empty(C.np,dtype=fType('real') )
			self.ip = NP.empty(C.np,dtype=fType('int') )
			self.npic = NP.empty( C.nx1, dtype=fType('int') )
		self.tres = diag.get_funit(C.iudm)
		diag.copyint(self.tres,C.iudm) #Not doing it this way causes some weird python error
		# velocity diagnostic
		if in1.ntv > 0:
			self.fv = NP.empty( (2*C.nmv+2,1), dtype=fType('real'), order='F' )
			self.fvm = NP.empty( (3,1), dtype=fType('real'), order='F')
			self.fv[0,:] = 8.0*in1.vtx
		# potential diagnostic
		if (in1.ntp > 0):
			self.sfield = NP.empty(C.nxe,dtype=fType('real') )
			if (in1.modesxp > C.nxh): 
				diag.copyint(C.nxh,in1.modesxp)
			self.pott = NP.empty(in1.modesxp, dtype=fType('complex') )
		#162: Skipped a bunch of stuff record time
		self.tt = diag.wtimer(C.ltime,1)
		C.time[0] = self.tt
		#write (iuot,*) 'initialization wall clock time = ', time, 'sec'
		self.dfname.write('initialization wall clock time = '+str(C.time[0])+"sec\n")
		self.fC = 0

	def run(self):
		self.fC += 1
		#! initialize charge density to background
		fld.isguard1(self.qe, C.qi0, C.nx, in1.inorder)
		#! deposit electron charge
		psm.igpost1(self.part, self.qe, C.np, in1.qme, C.tdpost, in1.inorder, in1.dopt)
		#! add guard cells
		fld.iaguard1(self.qe, C.nx, in1.inorder)
		#! velocity diagnostic
		if (in1.ntv > 0):
			C.it[0] = C.ntime/in1.ntv
			if (C.ntime==in1.ntv*C.it):
				#! calculate particle distribution function and moments
				diag.ivdist1(self.part,self.fv,self.fvm, C.np, C.nmv)
				#! display velocity distributions
				#diag.idisplayfv1(self.fv,self.fvm,' ELECTRON', C.ntime, C.nmv,2, C.irc)
				temp_obj = DrawVelocity( np.array(self.fv, copy=True) ) #copy the data
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj))
				#plt.plot(self.fv)
				#plt.show()
				if (C.irc==1):
					return
		#! phase space diagnostic
		if (in1.nts > 0):
			C.it[0] = C.ntime/in1.nts
			if (C.ntime==in1.nts*C.it):
				#! plot particles vx versus x
				diag.igrasp13(self.part, C.np,' ELECTRON', C.ntime, 999, C.nx, 2, 1, in1.npx, C.irc)
				if (C.irc==1):
					return
		#! transform charge to fourier space
		C.isign[0] = -1
		C.ws[0] = 0.0
		fft(self.qe, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
		#! potential diagnostic
		if (in1.ntp > 0):
			C.it[0] = C.ntime/in1.ntp
			if (C.ntime==in1.ntp*C.it):
				#! calculate potential in fourier space
				C.isign[0] = 1
				fld.ipois1(self.qe,self.sfield, C.isign, self.ffc, C.ws, C.nx, in1.inorder)
				#! store selected fourier modes
				gtmodes(self.sfield, self.pott, C.nx, in1.modesxp, in1.inorder)
				#! write diagnostic output
				#writebf(pott,modesxp,iup,nprec,order=LINEAR)
				#! transform potential to real space
				fft(self.sfield, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
				cguard(self.sfield, C.nx, in1.inorder)
				#! display potential
				#diag.idscaler1(self.sfield,' POTENTIAL', C.ntime, 999, 0, C.nx, C.irc, in1.inorder)
				temp_obj = DrawVelocity( np.array(self.sfield, copy=True) ) #copy the data
				temp_obj.plottype = "DRAWPOT"
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj))
				#plt.plot(sfield)
				#plt.show()
				if (C.irc[0]==1):
					return
		#! calculate force/charge in fourier space
		C.isign[0] = -1
		fld.ipois1(self.qe,self.fxe, C.isign, self.ffc, C.we, C.nx, in1.inorder)
		#! transform force/charge to real space
		C.isign[0] = 1
		fft(self.fxe, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
		cguard(self.fxe, C.nx, in1.inorder)
		#! push particles
		C.wke[0] =  0.0
		#modifies part, wke
		psm.igpush1(self.part,self.fxe, C.np, C.qbme, in1.dt, C.wke, C.tpush, C.nx, C.ipbc, in1.inorder, in1.popt)
		#! momentum diagnostic
		if (in1.ntm > 0): #ntm is zero.  Not fixing this code
			C.it[0] = C.ntime/in1.ntm
			#! calculate and print electron momentum
			C.it[0] = C.ntime - in1.ntm*C.it + 1
			if (C.it[0] > 1):
				C.it[0] = C.it[0] - in1.ntm
			if (C.it[0] >= 0):
				premoment1(self.part, C.ntime, C.np, C.ium, C.pxe, C.pye, C.pze, C.zero, C.zero, C.zero \
					,self.wx,self.wy,self.wz,in1.ndim,nprint=it)
		# sort electrons
		if (in1.sortime > 0):
			if ( C.ntime[0] % in1.sortime ==0):
				psm.isortp1x(self.part,self.pt,self.ip, C.np, self.npic, C.tsort,in1.inorder)
		C.itime[0] = C.itime + 1
		C.ntime[0] = C.itime + C.itime0
		self.tloop = diag.wtimer(C.ltime,1)
		C.time[0] = C.time + self.tloop
		if in1.ntw > 0:
			C.it[0] = C.itime[0]/in1.ntw
			if C.itime[0] == in1.ntw*C.it[0]:
				C.ws[0] = C.we[0] + C.wke[0]
				C.itw[0] += 1
				self.wt[C.itw[0]] = C.we, C.wke,0.0, C.ws  #(/we,wke,zero,ws/)
				print self.wt[C.itw[0]]

# Button definitions
ID_START = wx.NewId()
ID_STOP = wx.NewId()
# Define notification event for thread completion
EVT_RESULT_ID = wx.NewId()
def EVT_RESULT(win, func):
	"""Define Result Event."""
	win.Connect(-1, -1, EVT_RESULT_ID, func)
class ResultEvent(wx.PyEvent):
	"""Simple event to carry arbitrary result data."""
	def __init__(self, data):
		"""Init Result Event."""
		wx.PyEvent.__init__(self)
		self.SetEventType(EVT_RESULT_ID)
		self.data = data
		self.name = data.plottype

class DrawVelocity():
	def __init__(self,ydata):
		self.ydata = ydata
		self.plottype = "DRAWVELOCITY"

	def drawPlot(self, fig, axes):
		axes.plot(self.ydata)

class DrawEnergy():
	def __init__(self,data):
		self.edata = np.array(data,copy=True)
		self.plottype = "ENERGY"

	def drawPlot(self,fig, axes):
		axes.plot(edata[:,0])


class GraphStack:
	def __init__(self, ss, name, desc):
		self.description = desc
		self.name = name
		self.listenerQ = []  #List of registered listeners.  Unlimited size
		self.stack = deque() #internal stack
		self.stackSize = ss #set stack size

	def OnResult(self,event): #event handler
		self.stack.append(event)
		if len(self.stack) > self.stackSize:
			self.stack.popleft()
		self.broadcast(event)

	def AddListener(self, obj, pos=1): #Pos currently does nothing, but I may change that
		self.listenerQ.append([pos,obj])

	def RemoveListener(self,obj):
		rem = None
		for i in self.listenerQ:
			if i[1] == obj:
				rem = i
		if rem != None:
			self.listenerQ.remove(rem)
			print "Deleting Listener " + str(rem)

	def broadcast(self, event):
		for l in self.listenerQ: #dispatch result signal to all listener objects
			l[1].OnResult(event)

	def getRecent(self):
		if len(self.stack) > 0:
			return self.stack[-1]
		else:
			return None

class TestMe:
	def __init__(self,v):
		self.val = v

	def OnResult(self,event):
		print self.val

class LeftPanel(wx.Panel):
	def __init__(self, parent):
		self.mainframe = parent
		wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize)
		#vsizer1 = wx.BoxSizer(orient=wx.HORIZONTAL)
		self.createGraph()
		#vsizer1.Add(item=self.mycanvas, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
		#self.SetSizer(vsizer1)
		self.slopeStack = []
		self.currentEvent = None

	def createGraph(self):
		self.figure = matplotlib.figure.Figure()
		self.axes = self.figure.add_subplot(111)
		t = NP.arange(0.0,10,1.0)
		s = [0,1,0,1,0,2,1,2,1,0]
		self.y_max = 10
		self.axes.plot(t,s)
		self.mycanvas = FigureCanvas(self,-1,self.figure)
		self.mycanvas.mpl_connect('button_press_event',self.onclick)

	def OnResult(self, event):
		"""Show Result status."""
		if event.data is None:
			# Thread aborted (using our convention of None return)
			self.status.SetLabel('Computation aborted')
		else:
			#self.figure = matplotlib.figure.Figure()
			#self.axes = self.figure.add_subplot(111)
			self.currentEvent = event
			self.DrawPlot()
			

	def DrawPlot(self):
		self.axes.cla()
		self.currentEvent.data.drawPlot(self.figure, self.axes)
		self.mycanvas.draw()
		self.slopeStack = []

	def PlotLine(self,x1,x2):
		self.axes.plot([x1[0],x2[0]],[x1[1],x2[1]])
		self.mycanvas.draw()

	def onclick(self,event):
		if event.inaxes == None:
			return
		if len(self.slopeStack) < 2:
			self.slopeStack.append( np.array([event.xdata,event.ydata]) )
			self.mainframe.status.SetStatusText("First Point selected is "+str((event.xdata,event.ydata) )+ ". Select next point to find slope")
		if len(self.slopeStack) == 2:
			x1 = self.slopeStack[0]
			x2 = self.slopeStack[1]
			mv = x2 - x1
			m = mv[1]/mv[0]
			self.PlotLine(x1,x2)
			self.mainframe.status.SetStatusText("The slope is "+str(m) ) 
			self.slopeStack = []

class RightPanel(wx.Panel):
	def __init__(self, parent, sim):
		wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize)
		self.simframe = sim
		self.mainframe = parent
		vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
		mlb = self.makeListBox()
		newb = self.makeButton()
		newb2 = self.makeButtonNew()
		newb3 = self.makeResetButton()
		vsizer1.Add(item=mlb, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
		vsizer1.Add(item=newb, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
		vsizer1.Add(item=newb2, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
		vsizer1.Add(item=newb3, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
		self.SetSizer(vsizer1)
		self.initData()

	def makeListBox(self):
		lb1 = wx.ListBox(self,-1)
		lb2 = wx.ListBox(self,-1)
		self.listbox1 = lb1
		self.listbox2 = lb2
		hbox = wx.BoxSizer(orient=wx.HORIZONTAL)
		hbox.Add(item=lb1, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
		hbox.Add(item=lb2, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
		lb1.Bind(wx.EVT_LISTBOX_DCLICK, self.OnList1)
		lb2.Bind(wx.EVT_LISTBOX_DCLICK, self.OnList2)
		return hbox

	def OnList1(self, event):
		sel = self.listbox1.GetSelection()
		to = ResultEvent(self.lb1Data[sel])
		self.simframe.lpanel.OnResult(to)

	def OnList2(self, event):
		sel = self.listbox2.GetSelection()
		to = ResultEvent(self.lb2Data[sel])
		self.simframe.lpanel.OnResult(to)

	def makeButton(self):
		button1 = wx.Button(self,wx.NewId(),"Run!")
		button1.Bind(wx.EVT_BUTTON, self.OnStart )
		return button1

	def makeButtonNew(self):
		button1 = wx.Button(self,wx.NewId(),"New Frame")
		button1.Bind(wx.EVT_BUTTON, self.OnNewFrame)
		return button1	

	def makeResetButton(self):
		button1 = wx.Button(self,wx.NewId(),"Reset Simulation")
		button1.Bind(wx.EVT_BUTTON, self.OnReset)
		return button1	

	def initData(self):
		self.dataQueue = deque()
		self.maxSize = 8
		self.globalCounter = 0
		self.lb1Data = []
		self.lb2Data = []

	def OnStart(self, event):
		"""Start Computation."""
		# Trigger the worker thread unless it's already busy
		self.simframe.status.SetStatusText("Running")
		self.simframe.worker.run()

	def OnResult(self,event):
		self.globalCounter += 1
		self.dataQueue.append( (self.globalCounter, event.data) )
		if len(self.dataQueue) > self.maxSize:
			self.dataQueue.popleft()
		self.buildLists()

	def OnNewFrame(self,event):
		self.mainframe.windowList.append(NewFrame(self.mainframe))

	def buildLists(self):
		self.listbox1.Clear()
		self.listbox2.Clear()
		self.lb1Data = []
		self.lb2Data = []
		for i,l in enumerate(self.dataQueue):
			if l[1].plottype == "DRAWVELOCITY":
				self.listbox1.Append(str(l[0]))
				self.lb1Data.append(l[1])
			if l[1].plottype == "DRAWPOT":
				self.listbox2.Append(str(l[0]))
				self.lb2Data.append(l[1])

class Dumb(LeftPanel):
	def __init__(self, parent, dlist):
		self.parent = parent
		self.centralDispatcher = dlist
		#wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize, style=wx.SUNKEN_BORDER)
		LeftPanel.__init__(self,parent)
		self.emptyLabel = wx.StaticText(self,label="Right Click to select graph to display",size=wx.Size(320,240))
		self._mouseDownFlag = 0
		self.mycanvas.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
		self.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)

	def OnRightDown(self,event):
		menu = wx.Menu()
		for (i,g) in enumerate(self.centralDispatcher):
			ti = menu.Append( i, g.description )
			self.Bind(wx.EVT_MENU,self.PopupHandler, ti)
		print event.GetX()
		self.PopupMenu(menu, (event.GetX(), event.GetY()) )
		menu.Destroy()

	def PopupHandler(self,event):
		print event.GetId()
		for g in self.centralDispatcher:
			g.RemoveListener(self)
		self.centralDispatcher[event.GetId()].AddListener(self)
		self.emptyLabel.Hide()
		re = self.centralDispatcher[event.GetId()].getRecent()
		if re != None:
			self.currentEvent = re
			self.DrawPlot()

#I need an event to remove listeners when the window gets closed!
class NewFrame(wx.Frame):
	def __init__(self, parent):
		"""Create the MainFrame."""
		wx.Frame.__init__(self, None)
		self.InitData()
		self.InitMenu()
		self.status = self.CreateStatusBar()
		self.stf = parent
		#self.sizer = wx.BoxSizer(wx.HORIZONTAL)
		#self.lps = [LeftPanel(self,self.stf),LeftPanel(self,self.stf)]
		#for i in self.lps:
		#	self.sizer.Add(item=i, proportion=3, flag=wx.ALL | wx.EXPAND, border=2)
		#self.SetSizerAndFit(self.sizer)
		self.OnLayout1(None) #default layout
		self.Layout()
		self.Show()

	def InitData(self):
		self.displayAreas = deque()

	def PruneDisplays(self, num):
		while len(self.displayAreas) > num:
			toDie = self.displayAreas.pop()
			toDie.Hide()
			for d in self.stf.dispatchers:
				d.RemoveListener(toDie)
			print "Destroying "+str(toDie)
			toDie.Destroy()


	def _AddDisplay(self):
		toLive = Dumb(self, self.stf.dispatchers)
		#self.stf.dispatchers[0].AddListener(toLive) #THIS LINE IS DEBUG TO BE DELETED
		return toLive

	def RequestDisplays(self, num):
		if num > len(self.displayAreas):
			nd = num - len(self.displayAreas)
			for i in range(nd): #create nd new displays
				self.displayAreas.append(self._AddDisplay() )
			

	def InitMenu(self):
		menubar = wx.MenuBar()
		fileMenu = wx.Menu()
		layoutMenu = wx.Menu()
		fitem = fileMenu.Append(wx.ID_EXIT, 'Close', 'Close This Window')
		latem1 = layoutMenu.Append(wx.NewId(), '1 Graph')
		latem2h = layoutMenu.Append(wx.NewId(), '2 Graphs - Horizontal')
		latem2v = layoutMenu.Append(wx.NewId(), '2 Graphs - Vertical')
		latem3 = layoutMenu.Append(wx.NewId(), '3 Graphs')
		latem4 = layoutMenu.Append(wx.NewId(), '4 Graphs')
		menubar.Append(fileMenu, '&File')
		menubar.Append(layoutMenu, 'Layout')
		self.SetMenuBar(menubar)

		self.Bind(wx.EVT_MENU, self.OnQuit, fitem)
		self.Bind(wx.EVT_MENU, self.OnLayout1, latem1)
		self.Bind(wx.EVT_MENU, self.OnLayout2h, latem2h)
		self.Bind(wx.EVT_MENU, self.OnLayout2v, latem2v)
		self.Bind(wx.EVT_MENU, self.OnLayout3, latem3)
		self.Bind(wx.EVT_MENU, self.OnLayout4, latem4)
		self.Show(True)

	def OnQuit(self,event):
		self.Hide()

	def OnLayout1(self,event):
		self.PruneDisplays(1)
		self.RequestDisplays(1)
		self.sizer = wx.BoxSizer(wx.HORIZONTAL)
		for e in self.displayAreas:
			self.sizer.Add(item=e,flag=wx.ALL | wx.EXPAND | wx.CENTER)
		self.SetSizerAndFit(self.sizer)
		self.Layout()

	def OnLayout2h(self,event):
		self.PruneDisplays(2)
		self.RequestDisplays(2)
		self.sizer = wx.BoxSizer(wx.HORIZONTAL)
		for e in self.displayAreas:
			self.sizer.Add(item=e,flag=wx.ALL | wx.EXPAND | wx.CENTER)
		self.SetSizerAndFit(self.sizer)
		self.Layout()

	def OnLayout2v(self,event):
		self.PruneDisplays(2)
		self.RequestDisplays(2)
		self.sizer = wx.BoxSizer(wx.VERTICAL)
		for e in self.displayAreas:
			self.sizer.Add(item=e,flag=wx.ALL | wx.EXPAND | wx.CENTER)
		self.SetSizerAndFit(self.sizer)
		self.Layout()

	def OnLayout3(self,event):
		self.PruneDisplays(3)
		self.RequestDisplays(3)
		self.sizer = wx.BoxSizer(wx.VERTICAL)
		topRow = wx.BoxSizer(wx.HORIZONTAL)
		self.sizer.Add(item=topRow, flag= wx.ALL| wx.EXPAND)
		self.sizer.Add(item=self.displayAreas[2],flag=wx.ALL | wx.EXPAND | wx.CENTER)
		topRow.Add(item=self.displayAreas[0],flag=wx.ALL | wx.EXPAND | wx.CENTER)
		topRow.Add(item=self.displayAreas[1],flag=wx.ALL | wx.EXPAND | wx.CENTER)

		self.SetSizerAndFit(self.sizer)
		self.Layout()

	def OnLayout4(self,event):
		self.PruneDisplays(4)
		self.RequestDisplays(4)
		self.sizer = wx.BoxSizer(wx.VERTICAL)
		topRow = wx.BoxSizer(wx.HORIZONTAL)
		botRow = wx.BoxSizer(wx.HORIZONTAL)
		self.sizer.Add(item=topRow, flag= wx.ALL| wx.EXPAND)
		self.sizer.Add(item=botRow, flag=wx.ALL | wx.EXPAND | wx.CENTER)
		for i in range(2):
			topRow.Add(item=self.displayAreas[i],flag=wx.ALL | wx.EXPAND | wx.CENTER)
			botRow.Add(item=self.displayAreas[i+2],flag=wx.ALL | wx.EXPAND | wx.CENTER)

		self.SetSizerAndFit(self.sizer)
		self.Layout()		



# GUI Frame class that spins off the worker thread
class MainFrame(wx.Frame):
	"""Class MainFrame."""
	def __init__(self, parent, id):
		"""Create the MainFrame."""
		wx.Frame.__init__(self, parent, id, 'Thread Test')

		self.dispatchers = [GraphStack(3,"DRAWVELOCITY","Velocity Graph"), GraphStack(3,"DRAWPOT","Potenial Graph"), GraphStack(3,"Energy","Energy Graph") ]
		self.status = self.CreateStatusBar()	#create status bar
		self.status.SetStatusText("Hola")

		self.rpanel = RightPanel(self,self)	#create interface
		#self.lpanel = LeftPanel(self,self)
		self.sizer = wx.BoxSizer(wx.HORIZONTAL)
		#self.sizer.Add(item=self.lpanel, proportion=3, flag=wx.ALL | wx.EXPAND, border=2)
		self.sizer.Add(item=self.rpanel, proportion=1, flag=wx.ALL | wx.EXPAND, border=2)
		self.SetSizerAndFit(self.sizer)

		self.worker = RunSimulation(self)	#Initialize simulation
		EVT_RESULT(self,self.OnResult) #Link up events

		self.windowList = []
		self.windowList.append(NewFrame(self))
		#self.dispatchers[0].AddListener( self.windowList[0].lps[0] )
		#self.dispatchers[1].AddListener( self.windowList[0].lps[1] )

	def OnStart(self, event):
		"""Start Computation."""
		# Trigger the worker thread unless it's already busy
		self.worker.run()
		if not self.worker:
			self.status.SetLabel('Starting computation')

	def OnResult(self, event):
		#self.lpanel.OnResult(event)
		self.rpanel.OnResult(event)
		for d in self.dispatchers:  #Route events to proper stack
			if d.name == event.name:
				d.OnResult(event)
	

class MainApp(wx.App):
	"""Class Main App."""
	def OnInit(self):
		"""Init Main App."""
		self.frame = MainFrame(None, -1)
		self.frame.Show(True)
		self.SetTopWindow(self.frame)
		return True

#This is the original verbatim transcription of the Fortran code
#To use a GUI, we need to object-orientify things.  This code is left as a reference
#However it is no longer used.
def main():
	#Read in Fortran namelists
	loadNamelist(in1,'input1')			#Read into a fortran module
	loadNamelist(C,'declares')	#read in to python
	#Line 58
	diag.copyint(1,in1.idcode)
	diag.copyint(1,in1.psolve)
	diag.copyint(1,in1.ndim)
	#line 63
	cdrun = int(in1.idrun)
	dfname = open("output1."+str(int(cdrun)),'w')
	#Line 70
	# np = total number of electrons in simulation.
	C.np[0] = in1.npx + in1.npxb
	C.npx1[0] = in1.npx + 1
	C.nx[0] = 2**in1.indx
	C.nxh[0] = C.nx[0]/2
	C.nxe[0] = C.nx[0] + 4
	if in1.inorder == gl.linear:
		C.nxe[0] = C.nx[0] + 2
	elif in1.inorder == gl.cubic:
		C.nxe[0] = C.nx[0] + 6
	# dimension for index and sorting arrays
	C.nx1[0] = C.nx[0] + 1
	# nloop = number of time steps in simulation
	C.nloop[0] = in1.tend/in1.dt + .0001
	# part(1,n) = position x of particle n
	# part(2,n) = velocity vx of particle n
	part = NP.empty( (C.idimp,C.np), dtype=fType('real'), order='F' )
	qe = NP.empty( C.nxe, dtype=fType('real') )
	fxe= NP.empty( C.nxe , dtype=fType('real') )
	# ffc = form factor array for poisson solver
	ffc = NP.empty(C.nxh, dtype=fType('complex') )
	# mixup = array of bit reversed addresses for fft
	# sct = sine/cosine table for fft
	mixup = NP.empty(C.nxh,dtype=fType('int') )
	sct = NP.empty(C.nxh,dtype=fType('complex') )
	# open graphics device
	C.irc[0] = diag.open_graphs(in1.nplot)
	# initialize timer
	tt = diag.wtimer(C.ltime,-1)
	C.time[0] = tt
	# initialize constants
	C.itime[0] = C.itime0[0]
	C.ntime[0] = C.itime[0] + C.itime0[0]
	C.qbme[0] = in1.qme
	C.affp[0] = 1.0*C.nx[0]/(1.0*C.np[0])
	# set initial time
	diag.copyint(in1.dt*C.itime0, in1.t0)
	# set default diagnostic file names
	if in1.ntp > 0:
		fpname = 'potk1.'#//cdrun
	# energy time history
	if (in1.ntw > 0):
		wt = NP.empty( (  (C.nloop-1)/in1.ntw-(C.itime0/in1.ntw)+1 ,4), dtype=fType('real'), order='F' )
		C.itw[0] = 0
	#prepare fft tables
	pyfft1mod.fft1d.ifft1rxinit(mixup,sct,in1.indx)
	#calculate form factors
	fld.ipois1init(ffc, in1.ax, C.affp, C.nx)
	# initialize density profile and velocity distribution
	# background electrons
	if in1.npx > 0:
		distr(part,rightType(1),in1.npx,in1.vtx,in1.vx0,in1.npx,C.nx,C.ipbc)
	# beam electrons
	if in1.npxb > 0:
		distr(part, C.npx1, in1.npxb, in1.vtdx, in1.vdx, in1.npxb, C.nx, C.ipbc)
	# initialize charge density to background
	C.qi0 = -in1.qme/C.affp
	#! calculate initial electron momentum
	if (in1.ntm > 0):
		psm.initmomt1(part, C.np, C.pxe, C.pye, C.pze, in1.ndim)
	# sorting arrays
	if (in1.sortime > 0): 
		pt = NP.empty(C.np,dtype=fType('real') )
		ip = NP.empty(C.np,dtype=fType('int') )
		npic = NP.empty( C.nx1, dtype=fType('int') )
	tres = diag.get_funit(C.iudm)
	diag.copyint(tres,C.iudm) #Not doing it this way causes some weird python error
	# velocity diagnostic
	if in1.ntv > 0:
		fv = NP.empty( (2*C.nmv+2,1), dtype=fType('real'), order='F' )
		fvm = NP.empty( (3,1), dtype=fType('real'), order='F')
		fv[0,:] = 8.0*in1.vtx
	# potential diagnostic
	if (in1.ntp > 0):
		sfield = NP.empty(C.nxe,dtype=fType('real') )
		if (in1.modesxp > C.nxh): 
			diag.copyint(C.nxh,in1.modesxp)
		pott = NP.empty(in1.modesxp, dtype=fType('complex') )
	#162: Skipped a bunch of stuff record time
	tt = diag.wtimer(C.ltime,1)
	C.time[0] = tt
	#write (iuot,*) 'initialization wall clock time = ', time, 'sec'
	dfname.write('initialization wall clock time = '+str(C.time[0])+"sec\n")
	while C.nloop >= C.ntime:
		#! initialize charge density to background
		fld.isguard1(qe, C.qi0, C.nx, in1.inorder)
		#! deposit electron charge
		psm.igpost1(part, qe, C.np, in1.qme, C.tdpost, in1.inorder, in1.dopt)
		#! add guard cells
		fld.iaguard1(qe, C.nx, in1.inorder)
		#! velocity diagnostic
		if (in1.ntv > 0):
			C.it[0] = C.ntime/in1.ntv
			if (C.ntime==in1.ntv*C.it):
				#! calculate particle distribution function and moments
				diag.ivdist1(part,fv,fvm, C.np, C.nmv)
				#! display velocity distributions
				diag.idisplayfv1(fv,fvm,' ELECTRON', C.ntime, C.nmv,2, C.irc)
				#plt.plot(fv)
				#plt.show()
				if (C.irc==1):
					break
		#! phase space diagnostic
		if (in1.nts > 0):
			C.it[0] = C.ntime/in1.nts
			if (C.ntime==in1.nts*C.it):
				#! plot particles vx versus x
				diag.igrasp13(part, C.np,' ELECTRON', C.ntime, 999, C.nx, 2, 1, in1.npx, C.irc)
				if (C.irc==1):
					break
		#! transform charge to fourier space
		C.isign[0] = -1
		C.ws[0] = 0.0
		fft(qe, C.isign, mixup, sct, C.tfft, in1.indx, in1.inorder)
		#! potential diagnostic
		if (in1.ntp > 0):
			C.it[0] = C.ntime/in1.ntp
			if (C.ntime==in1.ntp*C.it):
				#! calculate potential in fourier space
				C.isign[0] = 1
				fld.ipois1(qe,sfield, C.isign, ffc, C.ws, C.nx, in1.inorder)
				#! store selected fourier modes
				gtmodes(sfield, pott, C.nx, in1.modesxp, in1.inorder)
				#! write diagnostic output
				#writebf(pott,modesxp,iup,nprec,order=LINEAR)
				#! transform potential to real space
				fft(sfield, C.isign, mixup, sct, C.tfft, in1.indx, in1.inorder)
				cguard(sfield, C.nx, in1.inorder)
				#! display potential
				diag.idscaler1(sfield,' POTENTIAL', C.ntime, 999, 0, C.nx, C.irc, in1.inorder)
				#plt.plot(sfield)
				#plt.show()
				if (C.irc[0]==1):
					break
		#! calculate force/charge in fourier space
		C.isign[0] = -1
		fld.ipois1(qe,fxe, C.isign, ffc, C.we, C.nx, in1.inorder)
		#! transform force/charge to real space
		C.isign[0] = 1
		fft(fxe, C.isign, mixup, sct, C.tfft, in1.indx, in1.inorder)
		cguard(fxe, C.nx, in1.inorder)
		#! push particles
		C.wke[0] =  0.0
		#modifies part, wke
		psm.igpush1(part,fxe, C.np, C.qbme, in1.dt, C.wke, C.tpush, C.nx, C.ipbc, in1.inorder, in1.popt)
		#! momentum diagnostic
		if (in1.ntm > 0): #ntm is zero.  Not fixing this code
			C.it[0] = C.ntime/in1.ntm
			#! calculate and print electron momentum
			C.it[0] = C.ntime - in1.ntm*C.it + 1
			if (C.it[0] > 1):
				C.it[0] = C.it[0] - in1.ntm
			if (C.it[0] >= 0):
				premoment1(part, C.ntime, C.np, C.ium, C.pxe, C.pye, C.pze, C.zero, C.zero, C.zero \
					,wx,wy,wz,ndim,nprint=it)
		# sort electrons
		if (in1.sortime > 0):
			if ( C.ntime[0] % in1.sortime ==0):
				psm.isortp1x(part,pt,ip, C.np,npic, C.tsort,in1.inorder)
		C.itime[0] = C.itime + 1
		C.ntime[0] = C.itime + C.itime0
		tloop = diag.wtimer(C.ltime,1)
		C.time[0] = C.time + tloop


if __name__ == "__main__":
	#main()
	app = MainApp(0)
	app.MainLoop()
