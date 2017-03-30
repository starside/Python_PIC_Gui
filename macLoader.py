# -----------------------------------------------------------------------
# 1D Electrostatic OpenMP PIC code
# written by Viktor K. Decyk and Joshua Kelly, UCLA
# copyright 2016, regents of the university of california
import sys
import math
import os
import numpy
import wx
import wx.stc
import f90nml
from types import *  # This is required for the rightType function

import Cocoa
path = os.path.dirname(Cocoa.NSBundle.mainBundle().bundlePath())
sys.path.append(path)

class MyFrame(wx.Frame):
    def __init__(self, parent, title):
        wx.Frame.__init__(self, parent, -1, title,
                          pos=(150, 150), size=(400, 220))    

        # Now create the Panel to put the other controls on.
        panel = wx.Panel(self)

        # and a few controls
        text = wx.StaticText(panel, -1, "Which program do you want to run?")
        text.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.BOLD))
        text.SetSize(text.GetBestSize())
        lbl = ["gui_mbeps1.py","gui_mbbeps1.py","gui_mdbeps1.py"]
        self.scripts = lbl
        self.rbox = wx.RadioBox(panel, -1, choices=lbl, majorDimension=1)
        sbox = wx.StaticBox(panel, -1, "Description")
        self.desc = wx.StaticText(sbox, -1, "Selected program description")
        self.wrapLen = 150
        self.desc.Wrap(self.wrapLen)
        button = wx.Button(panel, -1, "Run")
        # Use a sizer to layout the controls, stacked vertically and with
        # a 10 pixel border around eachs
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(text, 0, wx.ALL, 10)

        szr2 = wx.BoxSizer(wx.HORIZONTAL)
        szr2.Add(self.rbox, 0, wx.ALL, 10)
        szr2.Add(sbox, 0, wx.ALL, 10)
        sizer.Add(szr2, 0, wx.ALL, 10)
        sizer.Add(button, 0, wx.ALL, 10)
        panel.SetSizer(sizer)
        panel.Layout()
        self.panel = panel

        self.rbox.Bind(wx.EVT_RADIOBOX, self.OnRadio)
        button.Bind(wx.EVT_BUTTON, self.OnButton)
        self.updateDesc()

    def OnButton(self, event):
    	global efile
    	sel = self.rbox.GetSelection()
    	efile = path + '/' + self.scripts[sel]
    	self.Close()

    def OnRadio(self,event):
    	self.updateDesc()

    def updateDesc(self):
    	desc = ["Electrostatic Code", "Electromagnetic Code", "Darwin Code"]
    	sel = self.rbox.GetSelection()
    	self.desc.SetLabel(desc[sel])
    	self.desc.Wrap(self.wrapLen)
    	self.panel.Layout()


class MyApp(wx.App):
    def OnInit(self):
        frame = MyFrame(None, "PIC Loader")

        self.SetTopWindow(frame)
        #print "Print statements go to this stdout window by default."

        frame.Show(True)
        return True

def initGui():
	app = MyApp(redirect=True)
	app.MainLoop()

initGui()

#Check to see if global efile was set
try:
	efile
except:
	sys.exit(0)

#Execute the specified source file
try:
	global path
	path = os.path.dirname(Cocoa.NSBundle.mainBundle().bundlePath())
	sys.path.append(path + '/gui')
	sys.path.append(path + '/mbeps1.source')
	os.chdir(path)
	execfile(efile)
except:
	sys.stderr.write( "Could not open " + efile + "\n")

