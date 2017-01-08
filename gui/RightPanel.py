import wx
import wx.stc as stc
from InputEditor import *
from NewFrame import *
from Events import *

class RightPanel(wx.Panel):
    def __init__(self, parent, sim):
        wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize)
        self.alive = True
        self.simframe = sim
        self.mainframe = parent
        self.loader = parent.loader
        vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
        newb = self.makeButton()
        self.runOnceButton = newb
        newb2 = self.makeButtonNew()
        newb3 = self.makeInputButton()
        newb4 = self.makeResetButton()
        newb5 = self.makeRunLongButton()
        self.RunLongButton = newb5  #Need to bind/unbind this button
        self.timerText = wx.StaticText(self,-1,"Simulation time: ")
        vsizer1.Add(item=self.timerText, proportion=1, flag = wx.EXPAND | wx.ALL, border=10)
        vsizer1.Add(item=newb5, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)

        self.cbr = wx.CheckBox(self, -1, "Reverse Time?")
        self.cbr.SetValue(False)
        vsizer1.Add(item=self.cbr)
        self.cbr.Bind(wx.EVT_CHECKBOX, self.OnRunDir)

        #Fast Forward Feature
        ffs = wx.BoxSizer(orient=wx.HORIZONTAL)
        self.fftext = wx.StaticText(self,-1,"Fast Forward to: ")
        self.ffpoint = wx.TextCtrl(self,style=wx.TE_PROCESS_ENTER)
        self.ffstate = wx.StaticText(self,-1,"")
        ffs.Add(item=self.fftext, border=20)
        ffs.Add(item=self.ffpoint, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(ffs, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(self.ffstate)
        self.ffpoint.Bind(wx.EVT_TEXT_ENTER, self.OnFFChange)

        vsizer1.Add(item=newb, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
        vsizer1.Add(item=newb2, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
        vsizer1.Add(item=newb3)
        vsizer1.Add(item=newb4)

        self.pin = wx.CheckBox(self, -1, "Unpin This Window?")
        self.pin.SetValue(False)
        vsizer1.Add(item=self.pin)
        
        self.SetSizer(vsizer1)
        EVT_RUNSTEP(self, self.OnRunStep)
        wx.CallAfter( self.OnRunDir, None)  #Set reverse time variable in main code
        wx.PostEvent(self, RunStepEvent() )

    def OnFFChange(self,event):
        cv = self.ffpoint.GetValue()
        try:
            cvf = float(cv)
        except ValueError:  #Cant convert to a number
            cvf = 0.0
        if self.simframe.worker.simdata.has_key('tend'):
            te = self.simframe.worker.simdata['tend']
            if cvf > te:
                self.ffstate.SetLabel(str(cvf) + " is past end of sim.")
                return
        self.ffpoint.SetValue(str(cvf))
        self.ffstate.SetLabel("FF Point Changed To: "+str(cvf) )
        ns = VarChangeSignal()
        pEvents = self.simframe.pEvents
        ns.var["fastforward"] = cvf
        pEvents.put(ns)  #Send the signal to update

    def makeButton(self):
        button1 = wx.Button(self,wx.NewId(),"Run!")
        button1.Bind(wx.EVT_BUTTON, self.OnStart )
        return button1

    def makeButtonNew(self):
        button1 = wx.Button(self,wx.NewId(),"New Frame")
        button1.Bind(wx.EVT_BUTTON, self.OnNewFrame)
        return button1

    def makeInputButton(self):
        b1 = wx.Button(self,wx.NewId(), "Open Input File (input1)")
        b1.Bind(wx.EVT_BUTTON, self.OnOpenInput)
        return b1

    def makeResetButton(self):
        button1 = wx.Button(self,wx.NewId(),"Reset Simulation")
        button1.Bind(wx.EVT_BUTTON, self.simframe.OnReset)
        return button1  

    def makeRunLongButton(self):
        button1 = wx.ToggleButton(self,wx.NewId(),"Run Continuously")
        button1.Bind(wx.EVT_TOGGLEBUTTON, self.OnStartLong)
        return button1

    #A utitlity method to add new windows
    def makeNewFrame(self, layout, defaults):
        nf = NewFrame(self.mainframe, self.loader, self.mainframe,layout=layout,defaults=defaults)
        self.mainframe.windowList.append(nf)

    def OnRunDir(self,event):
        tv = 1
        if self.cbr.GetValue():
            tv = 1
        else:
            tv = 0

        ns = VarChangeSignal()
        pEvents = self.simframe.pEvents
        ns.var["timedirection"] = tv
        pEvents.put(ns)  #Send the signal to update

    def OnOpenInput(self, event):
        ie = InputEditor(self)
        ie.editor.loadInput()
        ie.Show()

    def OnStart(self, event):
        self.RunLongButton.Unbind(wx.EVT_BUTTON)
        self.runOnceButton.Unbind(wx.EVT_BUTTON)
        self.simframe.worker.iAmRunning = True #start the sim
        self.simframe.worker.runCounter = 1 #the number of times to run
        self.RunLongButton.SetValue(False)

    def OnStartLong(self, event):
        """Start Computation."""
        # Trigger the worker thread unless it's already busy
        self.RunLongButton.Unbind(wx.EVT_BUTTON)
        self.runOnceButton.Unbind(wx.EVT_BUTTON)
        #Toggle run state
        if hasattr(self.simframe.worker,'iAmRunning'):
            self.simframe.worker.iAmRunning = not self.simframe.worker.iAmRunning #Toggle value
        else:
            self.simframe.worker.iAmRunning = True
        #Decide if we run or not
        #if self.simframe.worker.iAmRunning:
        #   #Post event to call run
        #   wx.PostEvent(self, RunStepEvent() )

    def OnNewFrame(self,event):
        nf = NewFrame(self.mainframe, self.loader, self.mainframe)
        self.mainframe.windowList.append(nf)

    def OnRunStep(self,event):
        if not self.alive:
            return
        self.RunLongButton.Bind(wx.EVT_BUTTON, self.OnStartLong)
        self.runOnceButton.Bind(wx.EVT_BUTTON, self.OnStart)
        wx.Yield()
        self.simframe.worker.run()
        wx.PostEvent(self, RunStepEvent() )
