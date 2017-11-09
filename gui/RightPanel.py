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
        newb4c = self.makeCommandButton()
        newb4.Hide()
        newb5 = self.makeRunLongButton()
        self.RunLongButton = newb5  # Need to bind/unbind this button
        self.timerText = wx.StaticText(self, -1, "Simulation time: ")
        self.displayTime = 0    #This is updated by the main app
        vsizer1.Add(self.timerText, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)

        self.lblList = ['Continuous', 'Time Chunks']
        self.runmode = wx.RadioBox(self, label='Run Mode', pos=(80, 10), choices=self.lblList, majorDimension=1,
                                   style=wx.RA_SPECIFY_ROWS)
        self.runmode.Bind(wx.EVT_RADIOBOX, self.OnRunMode)
        vsizer1.Add(self.runmode)

        vsizer1.Add(newb5, proportion=1, flag=wx.EXPAND | wx.ALL, border=10)

        # Fast Forward Feature
        ffs = wx.BoxSizer(orient=wx.HORIZONTAL)
        self.fftext = wx.StaticText(self, -1, "Fast Forward to: ")
        self.ffpoint = wx.TextCtrl(self, style=wx.TE_PROCESS_ENTER)
        self.ffstate = wx.StaticText(self, -1, "")
        ffs.Add(self.fftext, border=20)
        ffs.Add(self.ffpoint, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(ffs, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(self.ffstate)
        self.ffpoint.Bind(wx.EVT_TEXT_ENTER, self.OnFFChange)

        rcs = wx.BoxSizer(orient=wx.HORIZONTAL)
        self.rcstext = wx.StaticText(self, -1, "Jump Time: ")
        self.rcspoint = wx.TextCtrl(self, style=wx.TE_PROCESS_ENTER)
        self.rcspoint.SetValue('1')
        rcs.Add(self.rcstext, border=20)
        rcs.Add(self.rcspoint, flag=wx.EXPAND | wx.ALL)
        vsizer1.Add(rcs, flag=wx.EXPAND | wx.ALL)

        vsizer1.Add(newb, proportion=1, flag=wx.EXPAND | wx.ALL, border=2)
        vsizer1.Add(newb2, proportion=1, flag=wx.EXPAND | wx.ALL, border=2)
        vsizer1.Add(newb3)
        vsizer1.Add(newb4)
        vsizer1.Add(newb4c)

        self.pin = wx.CheckBox(self, -1, "Unpin This Window?")
        self.pin.SetValue(True)
        vsizer1.Add(self.pin)
        self.SetSizer(vsizer1)
        self._updateRunMode()
        #Real time update varialbes
        self.realTimeVars = [] #Fortran input file parameters

        self.lastMode = None

        #Build Finit state machine
        self.fsm = FiniteStateMachine()
        self.fsm.addStateChange('1', '2sf', 'Running')        #Define transitions
        self.fsm.addStateChange('2sf', '1','Paused')
        self.fsm.addStateChange('1', '3','selTC')
        self.fsm.addStateChange('3', '1', 'selRC')
        self.fsm.addStateChange('3', '4', 'Running')
        self.fsm.addStateChange('4', '3', 'Paused')
        self.fsm.addStateChange('3', '3', 'Stay')

        self.fsm.defineState('1', [lambda : self.fsm1(None)])   #Define states
        self.fsm.defineState('2sf', [lambda : self.fsm2(None)])
        self.fsm.defineState('3', [lambda : self.fsm3(None)])
        self.fsm.defineState('4', [lambda : self.fsm4(None)])
        self.fsm.setState('1')                                  #Set the initial state
        #self.fsm.runState(self.fsm.currentState)                #Execute state
        wx.CallAfter(self.fsm.runState, self.fsm.currentState)

        EVT_RUNSTEP(self, self.OnRunStep)
        wx.PostEvent(self, RunStepEvent())

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

    def enableBoxNav(self, val=True):
        self.rcstext.Enable(val)
        self.rcspoint.Enable(val)
        self.rcspoint.Enable(val)
        self.fftext.Enable(val)
        self.ffpoint.Enable(val)
        self.ffstate.Enable(val)


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

    def OnRunMode(self, event):
        trans = None
        if self.lblList[0] == self.runmode.GetStringSelection():
            trans = self.fsm.pumpEvent('selRC')  # Send event to FSM
        elif self.lblList[1] == self.runmode.GetStringSelection():
            trans = self.fsm.pumpEvent('selTC')  # Send event to FSM
        self.fsm.runState(trans)  # get transition

    def OnFFChange(self, event):
        cv = self.ffpoint.GetValue()
        try:
            cvf = float(cv)
        except ValueError:  # Cant convert to a number
            cvf = 0.0
        if self.simframe.worker.simdata.has_key('tend'):
            te = self.simframe.worker.simdata['tend']
            if cvf > te:
                self.ffstate.SetLabel(str(cvf) + " is past end of sim.")
                return
        self.ffpoint.SetValue(str(cvf))
        self.ffstate.SetLabel("FF Point Changed To: " + str(cvf))
        ns = VarChangeSignal()
        pEvents = self.simframe.pEvents
        ns.var["fastforward"] = cvf
        pEvents.put(ns)  # Send the signal to update
        #return True

    def _StopFF(self):
        ns = VarChangeSignal()
        pEvents = self.simframe.pEvents
        ns.var["fastforward"] = 0
        pEvents.put(ns)  # Send the signal to update

    def makeButton(self):
        button1 = wx.Button(self, wx.NewId(), "Step")
        button1.Bind(wx.EVT_BUTTON, self.OnStart)
        return button1

    def makeButtonNew(self):
        button1 = wx.Button(self, wx.NewId(), "New Frame")
        button1.Bind(wx.EVT_BUTTON, self.OnNewFrame)
        return button1

    def makeInputButton(self):
        b1 = wx.Button(self, wx.NewId(), "Open Input File (input1)")
        b1.Bind(wx.EVT_BUTTON, self.OnOpenInput)
        return b1

    def makeCommandButton(self):
        b1 = wx.Button(self, wx.NewId(), "Open Command File (command1)")
        b1.Bind(wx.EVT_BUTTON, self.OnOpenCommand)
        return b1

    def makeResetButton(self):
        button1 = wx.Button(self, wx.NewId(), "Reset Simulation")
        button1.Bind(wx.EVT_BUTTON, self.fsm4)
        return button1

    def makeRunLongButton(self):
        self.runconText = "Run Continuously"
        button1 = wx.ToggleButton(self, wx.NewId(), self.runconText)
        return button1

    # A utitlity method to add new windows
    def makeNewFrame(self, layout, defaults):
        nf = NewFrame(self.mainframe, self.loader, self.mainframe, layout=layout, defaults=defaults)
        self.mainframe.windowList.append(nf)

    def OnOpenInput(self, event):
        ie = InputEditor(self, self.realTimeVars)
        ie.editor.loadInput()
        ie.Show()

    def OnOpenCommand(self, event):
        ie = InputEditor(self, self.realTimeVars, filename='command1')
        ie.editor.loadInput()
        ie.Show()

    def OnStart(self, event):
        self.simframe.worker.iAmRunning = True  # start the sim
        self.simframe.worker.runCounter = 1  # the number of times to run

    def OnStartLong(self, event):
        """
        Called when Run Continuously is called
        :param event:
        :return:
        """
        self.simframe.worker.iAmRunning = True
        self.OnFFChange(None)

    def OnPause(self, event):
        """
        Called when the code has paused
        :param event:
        :return:
        """
        self.simframe.worker.iAmRunning = False

    def OnRunChunk(self,event):
        cv = self.displayTime
        try:
            inc = float(self.rcspoint.GetValue())
        except:
            inc = None
        if inc is None:
            trans = self.fsm.pumpEvent('Stay')
            self.fsm.runState(trans)
            return
        cv += inc
        if cv > self.simframe.worker.simdata['tend']:
            cv = self.simframe.worker.simdata['tend']
        self.ffpoint.SetValue(str(cv))
        self.OnFFChange(None)
        self.ffpoint.SetValue(str(cv))
        self.simframe.worker.iAmRunning = True
        #self.RunLongButton.SetValue(self.simframe.worker.iAmRunning)


    def OnNewFrame(self, event):
        nf = NewFrame(self.mainframe, self.loader, self.mainframe)
        self.mainframe.windowList.append(nf)

    #The following states are for a finite state machine
    def fsm1(self, event):
        self.RunLongButton.Bind(wx.EVT_TOGGLEBUTTON, self.OnStartLong)
        self.runmode.Enable(True)   #Enable radio buttons
        self.runmode.SetStringSelection(self.lblList[0])
        self.enableFFBox(True)
        self.enableJumpBox(False)
        self.RunLongButton.SetLabel('Run Continuously')
        self.enableBoxNav(True)
        self.runOnceButton.Enable(True)
        self.Fit()

    def fsm2(self, event):
        self.RunLongButton.Bind(wx.EVT_TOGGLEBUTTON, self.OnPause)
        self.runmode.Enable(False)   #Enable radio buttons
        self.runmode.SetStringSelection(self.lblList[0])
        self.enableFFBox(True)
        self.enableJumpBox(False)
        self.RunLongButton.SetLabel('Pause')
        self.enableBoxNav(False)
        self.runOnceButton.Enable(False)
        self.Fit()

    def fsm3(self, event):
        self.RunLongButton.Bind(wx.EVT_TOGGLEBUTTON, self.OnRunChunk)
        self.RunLongButton.SetValue(False)
        self.runmode.Enable(True)   #Enable radio buttons
        self.runmode.SetStringSelection(self.lblList[1])
        self.enableJumpBox(True)
        self.enableFFBox(False)
        self.RunLongButton.SetLabel('Run Chunk')
        self.enableBoxNav(True)
        self.runOnceButton.Enable(True)
        self.Fit()

    def fsm4(self, event):
        self.RunLongButton.Bind(wx.EVT_TOGGLEBUTTON, self.OnPause)
        self.runmode.Enable(False)   #Enable radio buttons
        self.runmode.SetStringSelection(self.lblList[1])
        self.enableJumpBox(True)
        self.enableFFBox(False)
        self.RunLongButton.SetLabel('Pause')
        self.enableBoxNav(False)
        self.runOnceButton.Enable(False)
        self.Fit()


    # This is the main loop.  Instead of looping in a while, at the end of the function
    # it posts a wxEvent to call itself again.  Since this goes in to wxPython's event
    # queue, the gui remains responsive
    def OnRunStep(self, event):
        if not self.alive:
            self.Destroy()
            return
        #Generate toggle run state event
        if self.lastMode is None:
            self.lastMode = self.simframe.worker.iAmRunning
        if self.simframe.worker.iAmRunning != self.lastMode:
            if self.simframe.worker.iAmRunning:
                trans = self.fsm.pumpEvent('Running')
            else:
                self._StopFF()
                trans = self.fsm.pumpEvent('Paused')
            self.fsm.runState(trans)  # get transition
        self.lastMode = self.simframe.worker.iAmRunning

        #Run a time step
        wx.Yield()
        self.simframe.worker.run()
        wx.PostEvent(self, RunStepEvent())

class FiniteStateMachine:
    def __init__(self):
        self.nodes = dict()  #Matrix of transitions
        self.states = dict() #State definitions
        self.currentState = None

    def defineState(self,name, funclist):
        """
        Takes a list of functions to call, with no arguments
        :param funclist:
        :return:
        """
        self.states[name] = funclist

    def addStateChange(self, state1, state2, event):
        """
        Adds a transition path from state1 to state2, triggered by event.
        This is not bi-directional, as in it does not add a pth from state2 to state1
        :param state1:
        :param state2:
        :param event:
        :return:
        """
        if state1 not in self.nodes:
            self.nodes[state1] = dict()
        self.nodes[state1][event] = state2

    def setState(self, state):
        """
        Sets the current state to state
        :param state:
        :return:
        """
        if state in self.states:
            self.currentState = state
        else:
            raise('state must be defined before it is set as current')

    def pumpEvent(self, event):
        """
        Determine if there is a transition to a new event, and return it
        :param event:
        :return:
        """
        if self.currentState:
            s = self.nodes[self.currentState]
            if event in s:
                return s[event]
        return None

    def runState(self, state):
        if state is None:
            return
        if state in self.states:
            self.currentState = state
            funclist = self.states[state]
            for f in funclist:
                f()
