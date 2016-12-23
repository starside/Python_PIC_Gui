import threading
from lib import *
import numpy as NP

import sys
import math
import numpy
from libmpush1 import *
from fomplib import *
from fgraf1 import *
from dtimer import *

import sys

import matplotlib.pyplot as plt
import matplotlib

from Graphs import *
from Events import *


int_type = numpy.int32
double_type = numpy.float64
#if (minit1.fprecision()==0):
float_type = numpy.float32
complex_type = numpy.complex64

class RunSimulation(threading.Thread):
	def __init__(self,notify_window):
		"""Init Worker Thread Class."""
		threading.Thread.__init__(self)
		self._notify_window = notify_window
		self._want_abort = 0
		self.initFortran()
		self.start()

	def initFortran(self):
		#else:
		#  float_type = numpy.float64
		#  complex_type = numpy.complex128
		#  print "using double precision"

		# self.idimp = number of particle coordinates = 2
		# self.ipbc = particle boundary condition: 1 = periodic
		self.idimp = 2; self.ipbc = 1
		# self.wke/self.we = particle kinetic/electric field
		self.wke = numpy.zeros((1),float_type)
		self.we = numpy.zeros((1),float_type)
		# self.list = (true,false) = self.list of particles leaving tiles found in push
		self.list = True

		# declare scalars for standard code
		self.ws = numpy.zeros((1),float_type)

		# declare scalars for OpenMP code
		self.nppmx = numpy.empty((1),int_type)
		self.irc = numpy.zeros((1),int_type)

		# declare scalars for diagnostics
		# iuin = 8; iuot = 18; iudm = 19
		iude = 10; iup = 11; iuel = 12

		# declare and initialize timing data
		tinit = 0.0; tloop = 0.0
		itime = numpy.empty((4),numpy.int32)
		ltime = numpy.empty((4),numpy.int32)
		tdpost = numpy.zeros((1),float_type)
		tguard = numpy.zeros((1),float_type)
		tfft = numpy.zeros((1),float_type)
		tfield = numpy.zeros((1),float_type)
		tpush = numpy.zeros((1),float_type)
		tsort = numpy.zeros((1),float_type)
		tdiag = numpy.zeros((1),float_type)
		dtime = numpy.empty((1),double_type)

		# start timing initialization
		dtimer(dtime,itime,-1)
		# read nameself.list
		#open(unit=iuin,file='input1',form='formatted',status='old')
		iuin = open("input1","r")
		#read (iuin,input1)
		# override input data
		idcode = 1
		ndim = 1

		# debug; nameself.list not yet supported
		idrun = 1
		nvp = 0
		npx = 409600; npxb = 40960
		indx =  11
		mx = 32
		tend = 45.000; dt = 0.1
		qme = -1.0; vtx = 1.0; vty = 1.0; vtz = 1.0
		vx0 = 0.0; vy0 = 0.0; vz0 = 0.0
		mzf = 0
		ax = 0.912871
		nextrand = 0
		vtdx = 0.5; vtdy = 0.5; vtdz = 0.5
		vdx = 5.0; vdy = 5.0; vdz = 5.0
		xtras = 0.2
		#ntw = 1; ntde = 5; ntp = 5; ntel = 5; ntv = 5

		ntel = 0;
		ntde = 1
		ntt = 0
		nts = 0
		ntv = 1
		ntp = 0
		ntw = 1
		ntphi = 0

		nmv = 40
		#nst = 2; nprobt = 10
		#vtsx = 0.0; dvtx = 0.1
		nst = 3; nprobt = 0
		vtsx = 3.0; dvtx = 0.1
		modesxde = 41; modesxp = 41; modesxel = 41
		movion = 0
		amodex = 0.0; freq = 0.0; trmp = 0.0; toff = 0.0
		el0 = 0.0; er0 = 0.0
		ci = 0.1
		relativity = 0
		treverse = 0
		nplot = 0
		t0 = 0.0
		nderec = 0; nprec = 0; nelrec = 0

		# create string from idrun
		cdrun = str(idrun)
		# text output file
		fname = "output1." + cdrun
		#     open(unit=iuot,file=trim(fname),form='formatted',status='replace')
		iuot = open(fname,"w")
		#nvp = int(input("enter number of nodes: "))
		# initialize for shared memory parallel processing
		omplib.init_omp(nvp)

		# open graphics device
		self.irc[0] = graf1.open_graphs(nplot)

		# initialize scalars for standard code
		if (ntt > 0):
		   self.idimp = self.idimp + 1
		# np = total number of particles in simulation
		# nx = number of grid points in x direction
		np = npx + npxb; nx = int(math.pow(2,indx)); nxh = int(nx/2)
		nxe = nx + 2; nxeh = nxe/2
		# mx1 = number of tiles in x direction
		mx1 = int((nx - 1)/mx + 1)
		# nloop = number of time steps in simulation
		# ntime = current time step
		nloop = int(tend/dt + .0001); ntime = 0
		qbme = qme
		affp = float(nx)/float(np)

		# check for unimplemented features
		if (self.list):
		   if (self.ipbc != 1):
		      print "self.ipbc != 1 and self.list = True not yet supported"
		      self.list = False
		      print "self.list reset to False"

		# allocate data for standard code
		# part = particle array
		part = numpy.empty((self.idimp,np),float_type,'F')
		# qe = electron charge density with guard cells
		qe = numpy.empty((nxe),float_type,'F')
		# fxe = smoothed electric field with guard cells
		fxe = numpy.empty((nxe),float_type,'F')
		# ffc = form factor array for poisson solver
		ffc = numpy.empty((nxh),complex_type,'F')
		# mixup = bit reverse table for FFT
		mixup = numpy.empty((nxh),int_type,'F')
		# sct = sine/cosine table for FFT
		sct = numpy.empty((nxh),complex_type,'F')
		# kpic = number of particles in each tile
		kpic = numpy.empty((mx1),int_type,'F')

		# prepare fft tables
		mfft1.mfft1_init(mixup,sct,indx)
		# calculate form factors
		mfield1.mpois1_init(ffc,ax,affp,nx)
		# initialize different ensemble of random numbers
		if (nextrand > 0):
		   minit1.mnextran1(nextrand,ndim,np)
		# initialize particles
		it = npx + 1
		# background electrons
		if (npx > 0):
		   minit1.mudistr1(part,1,npx,nx,self.ipbc)
		   minit1.mvdistr1(part,1,vtx,vx0,npx)
		# beam electrons
		if (npxb > 0):
		   minit1.mudistr1(part,it,npxb,nx,self.ipbc)
		   minit1.mvdistr1(part,it,vtdx,vdx,npxb)

		# find number of particles in each of mx, tiles: updates kpic, self.nppmx
		minit1.mdblkp2(part,kpic,self.nppmx,mx,self.irc)

		# allocate vector particle data
		nppmx0 = int((1.0 + xtras)*self.nppmx)
		ntmax = int(xtras*self.nppmx)
		npbmx = int(xtras*self.nppmx)
		# ppart = tiled particle array
		ppart = numpy.empty((self.idimp,nppmx0,mx1),float_type,'F')
		# ppbuff = buffer array for reordering tiled particle array
		ppbuff = numpy.empty((self.idimp,npbmx,mx1),float_type,'F')
		# ncl = number of particles departing tile in each direction
		ncl = numpy.empty((2,mx1),int_type,'F')
		# ihole = location/destination of each particle departing tile
		ihole = numpy.empty((2,ntmax+1,mx1),int_type,'F')

		# copy ordered particle data for OpenMP: updates ppart and kpic
		mpush1.mpmovin1(part,ppart,kpic,mx,self.irc)

		# sanity check
		mpush1.mcheck1(ppart,kpic,nx,mx,self.irc)

		#Init GUI
		pc = PlasmaContext()
		pc.showGraphs(True)

		#Read the time direction from the GUI
		treverse = pc.timeDir.value

		# allocate diagnostic arrays
		# reverse simulation at end back to start

		if (treverse==1):
		   nloop = 2*nloop
		# energy time history
		if (ntw > 0):
		   it = int((nloop - 1)/ntw + 1); itw = 0
		# wt = energy time history array
		   wt = numpy.zeros((it,4+movion),float_type,'F')
		   timeHistory = numpy.zeros(it,float_type,'F')
		# allocate scratch arrays for scalar fields
		if ((ntde > 0) or (ntp > 0) or (ntel > 0)):
		   sfieldc = numpy.empty((nxh),complex_type,'F')
		   sfield = numpy.empty((nxe),float_type,'F')
		# electron density diagnostic
		if (ntde > 0):
		   fdename = "denek1." + cdrun
		   modesxde = int(min(modesxde,nxh+1))
		   denet = numpy.empty((modesxde),complex_type,'F')
		# open file: updates nderec and possibly iude
		   if (nderec==0):
		      nderec = numpy.zeros((1),int_type)
		      mdiag1.dafopennc1(denet,iude,nderec,fdename)
		# potential diagnostic
		if (ntp > 0):
		   fpname = "potk1." + cdrun
		   modesxp = int(min(modesxp,nxh+1))
		   pott = numpy.empty((modesxp),complex_type,'F')
		# open file: updates nprec and possibly iup
		   if (nprec==0):
		      nprec = numpy.zeros((1),int_type)
		      mdiag1.dafopennc1(pott,iup,nprec,fpname)
		# longitudinal efield diagnostic
		if (ntel > 0):
		   felname = "elk1." + cdrun
		   modesxel = int(min(modesxel,nxh+1))
		   elt = numpy.empty((modesxel),complex_type,'F')
		# open file: updates nelrec and possibly iuel
		   if (nelrec==0):
		      nelrec = numpy.zeros((1),int_type)
		      mdiag1.dafopennc1(elt,iuel,nelrec,felname)
		# velocity diagnostic
		if (ntv > 0):
		   sfv = numpy.empty((2*nmv+2,1,mx1+1),float_type,'F')
		   fvm = numpy.empty((3,1),float_type,'F')
		   sfv[0,:,:] = 2.0*int(max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx)))
		# trajectory diagnostic
		if (ntt > 0):
		   iprobt = numpy.empty((nprobt),numpy.int32)
		   mprobt = numpy.zeros((1),int_type)
		   mdiag1.setptraj1(ppart,kpic,iprobt,nst,vtx,vtsx,dvtx,np,mprobt)
		   nprobt = mprobt[0]
		   if (nprobt > 16777215):
		      print "nprobt overflow = ", nprobt
		      exit(1)
		   partt = numpy.empty((self.idimp,nprobt),float_type,'F')
		   if ((nst==1) or (nst==2)):
		      it = int((nloop - 1)/ntt + 1); itt = 0
		      partd = numpy.empty((it,self.idimp,nprobt),float_type,'F')
		   elif (nst==3):
		      fvt = numpy.empty((2*nmv+2,1),float_type,'F')
		      fvmt = numpy.empty((3,1),float_type,'F')
		      fvt[0,:] = 2.0*int(max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx)))

		# initialization time
		dtimer(dtime,itime,1)
		tinit = tinit + float(dtime)
		# start timing loop
		dtimer(dtime,ltime,-1)


		phikw = []
		phikw_time = []
		phikw_dta = []


		iuot.write("program mbeps1\n")
		curtime = 0.0
		#LEave Here
		self.iAmRunning = False

	def run(self):
		while self.step() == 0:
			True


	def step(self):
		#Python Changes
		self.fC += 1
		postCount = 0



		#Python Enhancements
		wx.PostEvent(self._notify_window, SimTimeEvent(self.curTime) )
		self.curtime += in1.dt
		return postCount