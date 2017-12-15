import wx
import wx.stc as stc
from lib import *

from Signals import *


class TC: pass


class EditorPart(stc.StyledTextCtrl):
    def __init__(self, parent, style=wx.SIMPLE_BORDER, realTimeVars = []):
        stc.StyledTextCtrl.__init__(self, parent, style=style)
        self.parent = parent
        self.SetMinSize((500, 500))
        # List of parameters highlighred red than can be updated during the simulation run
        self.realTimeUpdate = realTimeVars
        self.presentVars = []
        self.filename = "input1"

    def loadInput(self):
        self.ClearAll()
        with open(self.filename, "r") as fp:
            rb = fp.read()
        self.StyleSetForeground(1, wx.NamedColour('red'))
        self.AddText(rb)
        for p in self.realTimeUpdate:
            s = rb.lower().find(p.lower())
            if s != -1:
                l = len(p)
                self.StartStyling(s, 31)
                self.SetStyling(l, 1)
                self.presentVars.append(p)

    def saveInput(self):
        with open(self.filename, "w") as fp:
            fp.write(self.GetText())

    def updateChanges(self):
        to = TC()  # create temp object
        nmo = ""
        with open(self.filename) as fp:  # read original
            nmo = fp.read()
        with open(".temp_" + self.filename, "w") as fp:  # backup
            fp.write(nmo)
        self.saveInput()  # save namelist
        # loadNamelist(to, "input1") #load changes in to software
        loadNamelistRaw(to, self.filename)
        with open(self.filename, "w") as fp:  # resote original
            fp.write(nmo)
        ns = VarChangeSignal()
        pEvents = self.parent.parent.simframe.pEvents  # Kind of a not flexible way to do this shit
        for p in self.presentVars:
            ns.var[p.lower()] = getattr(to, p.lower())
            """atv = getattr(to,p.lower())
            if atv.dtype.kind == "i": #integer
                diag.copyint(atv, getattr(in1,p.lower()) )
            elif atv.dtype.kind == "f":
                diag.copyreal(atv, getattr(in1,p.lower()) )"""
        pEvents.put(ns)


class InputEditor(wx.Frame):
    def __init__(self, parent, realTimeVars = [], filename='input1'):
        wx.Frame.__init__(self, parent, -1, style=wx.FRAME_FLOAT_ON_PARENT | wx.DEFAULT_FRAME_STYLE)
        self.editor = EditorPart(self, realTimeVars=realTimeVars)
        self.editor.filename = filename
        self.SetTitle(self.editor.filename)
        self.parent = parent
        # define nav bar
        navb = wx.BoxSizer(wx.HORIZONTAL)
        saveb = wx.Button(self, -1, "Save to " + self.editor.filename)
        saveb.Bind(wx.EVT_BUTTON, self.OnSave)
        upb = wx.Button(self, -1, "Update Changes")
        upb.Bind(wx.EVT_BUTTON, self.OnUpdate)
        navb.Add(saveb, flag=wx.ALIGN_LEFT)
        # navb.AddStretchSpacer(1)
        stxt = wx.StaticText(self, -1, "Red variables can be updated while running")
        stxt.SetForegroundColour(wx.Colour(255, 0, 0))
        navb.Add(stxt, flag=wx.ALIGN_CENTER, proportion=1)
        navb.Add(upb, flag=wx.ALIGN_RIGHT | wx.EXPAND)

        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.sizer.Add(navb, flag=wx.EXPAND, border=2)
        self.sizer.Add(self.editor, flag=wx.EXPAND | wx.ALL, proportion=1)

        self.statusbar = self.CreateStatusBar()

        self.SetSizerAndFit(self.sizer)
        self.editor.loadInput()

    def OnSave(self, event):
        self.editor.saveInput()
        self.statusbar.SetStatusText("Save text to file " + self.editor.filename)

    def OnUpdate(self, event):
        self.editor.updateChanges()
        self.statusbar.SetStatusText("Updated Changes in red highlights but DID NOT SAVE CHANGES TO FILE!")
