import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import wx
import os
from threading import Thread
from multiprocessing import Process, Pipe

import Graphs
import GraphStack
import NewFrame
from Events import *
from defaults import *

#import new_beps1gl

from ProceduralInterface import *

#Lowest Level use of library.  Operates on plotlib contexts directly
"""td1 = np.ones((100,2))
figure = plt.figure()
axes = figure.add_subplot(111)
dv1 = Graphs.DrawVelocity(td1)
dv1.drawPlot(figure,axes)
plt.show()"""

tlf = None

#Try the high level interface
"""def initGui(q):
	if wx.GetApp() == None:
		app = new_beps1gl.MainApp(0,q)
		app.MainLoop()
		return app
	else:
		print "Cannot create multiple wxApp contexts"
		return False

#t = Thread(target=initGui)
#t.start()
parent_conn, child_conn = Pipe()
p = Process(target=initGui, args=(child_conn,) )
p.start()"""


td1 = np.ones((100,2))

c = PlasmaContext()

while True:
	print "SV"
	c.showVelocity(td1)
	#parent_conn.send(dv1)
	#wx.PostEvent(tlf.frame, ResultEvent(dv1, 0))
