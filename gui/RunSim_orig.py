import threading
from lib import *
import numpy as NP


from FortranState import *

from Graphs import *
from Events import *



QUIET = True

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
		# sct = sine/cosine table for fft()
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

		#Non fotran python enhancements
		self.energySeries = []
		self.entimeSeries = []
		self.curTime = 0.0
		self.phikw = []  #stores all fourier transforms of potential
		self.phikw_time = []  #store the time at which the snapshot occurred
		self.phikw_dta = [] #array of time steps
		self._tdataSize = 0 #memory use of phikw
		self.phikw_maxmem = 0.01 #maximum number of megabytes to store before discarding old data.  Currently not implemented
		self.iAmRunning = False

	def run(self):
		while self.step() == 0:
			True


	def step(self):
		#Python Changes
		self.fC += 1
		postCount = 0
		#Fortran transcription
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
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
				postCount += 1
				#plt.plot(self.fv)
				#plt.show()
				if (C.irc==1):
					return
		#! phase space diagnostic
		if (in1.nts > 0):
			C.it[0] = C.ntime/in1.nts
			if (C.ntime==in1.nts*C.it):
				#! plot particles vx versus x
				#diag.igrasp13(self.part, C.np,' ELECTRON', C.ntime, 999, C.nx, 2, 1, in1.npx, C.irc)
				print np.sum(self.part)
				temp_obj = DrawPhase( np.array(self.part, copy=True) ) #copy the data
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
				postCount += 1
				if (C.irc==1):
					return
		#! transform charge to fourier space
		C.isign[0] = -1
		C.ws[0] = 0.0
		fft(self.qe, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)

		#! potential diagnostic
		C.isign[0] = 1
		fld.ipois1(self.qe,self.sfield, C.isign, self.ffc, C.ws, C.nx, in1.inorder) 
		#! store selected fourier modes
		gtmodes(self.sfield, self.pott, C.nx, in1.modesxp, in1.inorder) #Copy sfield or pott here
		self.phikw.append( NP.array(self.pott, copy=True) )  #add data to phikw
		self.phikw_time.append( self.curTime )
		self.phikw_dta.append(in1.dt)
		self._tdataSize += sys.getsizeof(self.pott)
		if (in1.ntphi > 0):  #update graphs
			C.it[0] = C.ntime/in1.ntphi
			if (C.ntime==in1.ntphi*C.it):
				#plt.plot()
				temp_obj = DrawPhi([np.array(self.phikw_time), self.phikw, self.phikw_dta],in1.dt)
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
		#! write diagnostic output
		#! transform potential to real space
		fft(self.sfield, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
		cguard(self.sfield, C.nx, in1.inorder)
		if (in1.ntp > 0):  #update graphs
			C.it[0] = C.ntime/in1.ntp
			if (C.ntime==in1.ntp*C.it):
				#! display potential
				#diag.idscaler1(self.sfield,' POTENTIAL', C.ntime, 999, 0, C.nx, C.irc, in1.inorder)
				temp_obj = DrawPotential( np.array(self.sfield, copy=True) ) #copy the data
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
				postCount += 1
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
		if in1.ntw > 0:  #energy diagnostic
			C.it[0] = C.itime[0]/in1.ntw
			if C.itime[0] == in1.ntw*C.it[0]:
				C.ws[0] = C.we[0] + C.wke[0]
				C.itw[0] += 1
				#self.wt[C.itw[0]] = C.we, C.wke,0.0, C.ws  #(/we,wke,zero,ws/)
				self.entimeSeries.append(self.curTime)
				self.energySeries.append(C.we[0])
				temp_obj = DrawEnergy( np.array(self.energySeries) , np.array(self.entimeSeries) ) #copy the data
				wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
		#Python Enhancements
		wx.PostEvent(self._notify_window, SimTimeEvent(self.curTime) )
		self.curTime += in1.dt
		return postCount