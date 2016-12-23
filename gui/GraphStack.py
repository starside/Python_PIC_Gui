from collections import deque


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

	def countListeners(self):
		return len(self.listenerQ)

class DispList:
	def __init__(self):
		self.dl = []

	def __iter__(self):
		return iter(self.dl)

class Dispatcher:
	def initStack(self):
		self.dispatchers = [GraphStack(3,"DRAWVELOCITY","Velocity Graph"), GraphStack(3,"DRAWPOT","Potenial Graph"), \
			GraphStack(3,"ENERGY","Energy Graph"), GraphStack(3,"DRAWPHASE","Phase Plot"), GraphStack(3,"DRAWFASTPHASE","Phase Histogram (Faster)"), GraphStack(3,"DRAWPHI","Phi(k,w)"), \
			GraphStack(3,"DRAWTRAJ","Trajectory"),GraphStack(3,"DRAWMULTITRAJ","Multiple Trajectories"), GraphStack(3,"DRAWDENSE","Electron Density"), GraphStack(3,"LONEFIELD","E-Field Longitudinal"),
			GraphStack(3,"ENTROPY","Entropy") ]


	def OnResult(self, event):
		for d in self.dispatchers:  #Route events to proper stack
			if d.name == event.name:
				d.OnResult(event)