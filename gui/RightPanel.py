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

        self.lblList = ['Continuous', 'Time Chunks']
        self.runmode = wx.RadioBox(self, label='Run Mode', pos=(80, 10), choices=self.lblList, majorDimension=1, style=wx.RA_SPECIFY_ROWS)
        self.runmode.Bind(wx.EVT_RADIOBOX, self.OnRunMode)
        vsizer1.Add(item=self.runmode)

        vsizer1.Add(item=newb5, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)


        self.cbr = wx.CheckBox(self, -1, "Reverse Time?")
        self.cbr.SetValue(False)
        vsizer1.Add(item=self.cbr)
        self.cbr.Bind(wx.EVT_CHECKBOX, self.OnRunDir)
        self.cbr.Hide()

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

        rcs = wx.BoxSizer(orient=wx.HORIZONTAL)
        self.rcstext = wx.StaticText(self, -1, "Jump Time: ")
        self.rcspoint = wx.TextCtrl(self, style=wx.TE_PROCESS_ENTER)
        rcs.Add(item=self.rcstext, border=20)
        rcs.Add(item=self.rcspoint, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(rcs, flag=wx.EXPAND | wx.ALL)

        vsizer1.Add(item=newb, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
        vsizer1.Add(item=newb2, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)
        vsizer1.Add(item=newb3)
        vsizer1.Add(item=newb4)

        self.pin = wx.CheckBox(self, -1, "Unpin This Window?")
        self.pin.SetValue(True)
        vsizer1.Add(item=self.pin)
        self.SetSizer(vsizer1)
        self._updateRunMode()

        EVT_RUNSTEP(self, self.OnRunStep)
        wx.CallAfter( self.OnRunDir, None)  #Set reverse time variable in main code
        wx.PostEvent(self, RunStepEvent() )

    def enableJumpBox(self, val=True):
        if val:
            self.rcstext.Show()
            self.rcspoint.Show()
            self.rcspoint.Show()
        else:
            self.rcstext.Hide()
            self.rcspoint.Hide()
            self.rcspoint.Hide()

    def enableFFBox(self, val=True):
        if val:
            self.fftext.Show()
            self.ffpoint.Show()
            self.ffstate.Show()
        else:
            self.fftext.Hide()
            self.ffpoint.Hide()
            self.ffstate.Hide()

    def _updateRunMode(self):
        sel = self.runmode.GetStringSelection()
        if sel == self.lblList[1]:
            self.enableFFBox(False)
            self.enableJumpBox(True)
            self.runconText = "Run Chunk"
        elif sel == self.lblList[0]:
            self.enableFFBox(True)
            self.enableJumpBox(False)
            self.runconText = "Run Continuously"

        self.Fit()

    def OnRunMode(self,event):
        if not self.simframe.worker.iAmRunning:
            print self.runmode.GetStringSelection()
            self._updateRunMode()

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
        button1 = wx.Button(self,wx.NewId(),"Step")
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
        self.runconText = "Run Continuously"
        button1 = wx.ToggleButton(self,wx.NewId(), self.runconText)
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

    def StartedRunning(self):
        True

    def StoppedRunning(self):
        True

    def OnStart(self, event):
        self.RunLongButton.Unbind(wx.EVT_BUTTON)
        self.runOnceButton.Unbind(wx.EVT_BUTTON)
        self.simframe.worker.iAmRunning = True #start the sim
        self.simframe.worker.runCounter = 1 #the number of times to run
        self.RunLongButton.SetValue(False)

    #Method called at the top of the main event loop
    def beforeMainLoop(self):
        self.RunLongButton.Unbind(wx.EVT_BUTTON)
        self.runOnceButton.Unbind(wx.EVT_BUTTON)

    #Method called after mainloop started
    def afterMainLoop(self):
        if self.simframe.worker.iAmRunning:
            self.RunLongButton.SetLabel('Pause')
        else:
            self.RunLongButton.SetLabel(self.runconText)
        self.runmode.Enable(not self.simframe.worker.iAmRunning)
        self.RunLongButton.Bind(wx.EVT_BUTTON, self.OnStartLong)
        self.runOnceButton.Bind(wx.EVT_BUTTON, self.OnStart)

    def OnStartLong(self, event):
        """Start Computation."""
        #Check to see our current runMode
        if self.runmode.GetStringSelection() == self.lblList[1] and self.rcspoint.GetValue() is not None:
            try:
                cv = float(self.ffpoint.GetValue())
            except:
                cv = None
            try:
                inc = float(self.rcspoint.GetValue())
            except:
                inc = None
            if cv is None:
                cv = 0
            if inc is None:
                inc = 0
            cv += inc
            self.ffpoint.SetValue(str(cv))
            self.OnFFChange(None)
        self.beforeMainLoop()

        #Toggle run state
        if hasattr(self.simframe.worker,'iAmRunning'):
            self.simframe.worker.iAmRunning = not self.simframe.worker.iAmRunning #Toggle value
        else:
            self.simframe.worker.iAmRunning = True
        self.runmode.Enable(not self.simframe.worker.iAmRunning)

    def OnNewFrame(self,event):
        nf = NewFrame(self.mainframe, self.loader, self.mainframe)
        self.mainframe.windowList.append(nf)

    #This is the main loop.  Instead of looping in a while, at the end of the function
    #it posts a wxEvent to call itself again.  Since this goes in to wxPython's event
    #queue, the gui remains responsive
    def OnRunStep(self,event):
        if not self.alive:
            self.Destroy()
            return
        self.afterMainLoop()
        wx.Yield()
        self.simframe.worker.run()
        wx.PostEvent(self, RunStepEvent() )
