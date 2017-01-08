from lib import *
import numpy as NP
import cPickle
import time

from Graphs import *
from Events import *

from multiprocessing import Queue
import Queue as QE

class PipeSimulation():
	def __init__(self,notify_window, pipe, que, timedir, outq):
		"""Init Worker Thread Class."""
		#threading.Thread.__init__(self)
		#self.unproc = []
		self._notify_window = notify_window
		self._want_abort = 0
		self.pipe = pipe
		self.que = que
		self.outq = outq
		self.timeDir = timedir
		self._pollrate = 1.0/30.0  #times per second to poll for input.  Set to 0 for no delay
		self.myq = []
		self.guiq = []  #GUI CONTROL QUEUE
		self.runCounter = -1
		self.simdata = {}
		self.initFortran()
		#self.start()

	def initFortran(self):
		self.iAmRunning = False
		self.fC = 0
		self.curTime = 0

	def run(self):
		while self.step() == 0:
			True

	def sigPath(self,temp_obj):
		if temp_obj.signame == "OPENFRAME":
			wx.PostEvent(self._notify_window, ControlEvent(temp_obj, self.curTime))
		elif temp_obj.signame == "SETTIME":
			wx.PostEvent(self._notify_window, SimTimeEvent(temp_obj.time))
		elif temp_obj.signame == "CLEARGRAPHSTACK":
			_tcs = ClearGraphStackEvent()
			if hasattr(temp_obj,"codename"):  #pass on info related to adding new graphstack listeners
				_tcs.codename = temp_obj.codename
				_tcs.desc = temp_obj.desc
			wx.PostEvent(self._notify_window, _tcs)
		elif temp_obj.signame == "SIMDATA":
			self.simdata = temp_obj.data

	def controlPath(self, temp_obj):
		if temp_obj == "RUNCONTROL":
			if self.runCounter > 0:
				self.iAmRunning = True
			if self.runCounter == 0:
				self.iAmRunning = False
			self.runCounter += -1
			if self.runCounter < 0:
				self.runCounter = -1
		elif temp_obj == "PAUSE":
			self.iAmRunning = False
			self._notify_window.rpanel.RunLongButton.SetValue(False)
			self.guiq.remove(temp_obj)

	def dataPath(self, temp_obj):
		#Is the sim running?
		#if not self.iAmRunning:
		#	time.sleep(0.1)  #Introduce a 1/10 second delay in pausing/unpausing.  This is so
		#	return 1
		#Try to set curTime
		try:
			self.curTime = temp_obj._tackOnTime
		except:
			True
		wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))

	def step(self):
		#Python Changes
		self.fC += 1
		postCount = 1
		#if self.pipe.poll(self._pollrate):  #If there is data for us
		#temp_obj = self.pipe.recv()
		#wx.PostEvent(self._notify_window, ResultEvent(temp_obj, self.curTime))
		#print self.fC
		#see if we need to read data from queue
		#if not self.que.empty():
		if len(self.myq) == 0 and len(self.guiq) == 0:
			try:
				rdo = cPickle.loads(self.que.get())
			except:
				wx.Yield()
				return 1
			if type(rdo) == str or hasattr(rdo,'signame'):  #Split to two different ques
				self.guiq.append(rdo)
			else:
				self.myq.append(rdo)
		if len(self.myq) == 0 and len(self.guiq) == 0:
			return 1 #Nothing to do

		#Read control q.  The que is always responsive
		if len(self.guiq) > 0:
			temp_obj = self.guiq[0]
			if hasattr(temp_obj, 'signame'): #Check to see if object is a graph of a signal
				self.sigPath(temp_obj)
				self.guiq.remove(temp_obj)
			else:
				self.controlPath(temp_obj)
				time.sleep(1.0/30.0)
				if self.iAmRunning:  #The will cause getEvents to block unless iAmRunning is True
					self.pipe.send("GoR")
					self.guiq.remove(temp_obj)

		#Read queue myq.  This que can be paused
		if len(self.myq) > 0:
			temp_obj = self.myq[0]
			self.myq.remove(temp_obj)
			self.dataPath(temp_obj)
			self.pipe.send("Go")
			
		return postCount
