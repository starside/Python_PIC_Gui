import wx
import wx.stc as stc
import numpy as NP
from distutils import spawn

from subprocess import Popen, PIPE
import copy
# import Image
import os, sys

from Events import *
import Graphs
import Contexts


# class for the frame handling
class RecordPanel(wx.Frame):
    currentlyWritingFiles = []  # Class variable, containing names of files currently recording

    def __init__(self, parent):
        wx.Frame.__init__(self, parent, -1, 'Movie Options', style=wx.FRAME_FLOAT_ON_PARENT | wx.DEFAULT_FRAME_STYLE)
        self.parent = parent
        self.SetupControls()
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Show()

    def SetupControls(self):
        self.sizer = wx.BoxSizer(orient=wx.VERTICAL)
        hs1 = wx.BoxSizer(orient=wx.HORIZONTAL)

        rec, text, overwrite = self.parent.getRecordStatus()

        self.cbox2 = wx.CheckBox(self, -1, "Overwrite Existing Movie File")
        self.cbox2.SetValue(overwrite)
        self.sizer.Add(self.cbox2)

        lbl1 = wx.StaticText(self, label="Movie File Name:")
        self.saveName = wx.TextCtrl(self, size=(140, -1), value=text)
        hs1.Add(lbl1)
        hs1.Add(self.saveName)

        self.cbox1 = wx.ToggleButton(self, -1, "Record/Pause")
        self.cbox1.Bind(wx.EVT_TOGGLEBUTTON, self.OnClickRecord)
        self._RecButtonValue()
        self.sizer.Add(hs1)
        self.sizer.Add(self.cbox1)

        self.SetSizerAndFit(self.sizer)

    def _RecButtonValue(self):
        rec, text, overwrite = self.parent.getRecordStatus()
        self.cbox1.SetValue(rec)
        if rec:
            self.cbox1.SetLabel("Pause")
        else:
            self.cbox1.SetLabel("Record")

    def OnClose(self, event):
        self.Hide()
        self.Destroy()


    def OnClickRecord(self, event):
        svname = self.saveName.GetValue()
        recval = self.cbox1.GetValue()
        overwrite = self.cbox2.GetValue()
        if recval and self.fileExists(svname) and not overwrite:  # check if we can overwrite
            wx.MessageBox('File ' + svname + " already exists.  Enable overwrite.", 'Cannot Record',
                          wx.OK | wx.ICON_INFORMATION)
        else:  # No overwriting flags
            if not self.addActiveWritingFile(svname) and recval:  # currently writing to stream
                wx.MessageBox('Another video is currently recording to file ' + svname + ".  Not recording.",
                              'Cannot Record', wx.OK | wx.ICON_INFORMATION)
            else:
                self.parent.setRecordStatus(recval, svname, overwrite)
        if not recval:  # Stopping recording
            try:
                self.currentlyWritingFiles.remove(svname)
            except ValueError:
                True
        self._RecButtonValue() #Update button

    def addActiveWritingFile(self, fname):
        if fname in self.currentlyWritingFiles:
            return False
        else:
            self.currentlyWritingFiles.append(fname)
            return True

    def fileExists(self, fname):
        return os.path.isfile(fname)


"""class MyCustomToolbar(NavigationToolbar2Wx):
    def __init__(self, plotCanvas):
        # create the default toolbar
        NavigationToolbar2Wx.__init__(self, plotCanvas)
        # remove the unwanted buttons
        self.DeleteToolByPos(1)
        self.DeleteToolByPos(1)
        self.DeleteToolByPos(5)
"""


class LeftPanel(wx.Panel):
    def __init__(self, parent):
        self.mainframe = parent
        wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize)
        self.createGraph()
        self.slopeStack = []
        self.slopeMousePointer = None
        self.measuring = False
        self.currentEvent = None
        self.arbGraphParameters = {"axesType": "Linear-Linear"}
        self.newCP = None
        # movie recording defaults
        self.recordVideo = False
        self.movieFileName = "Movie.avi"
        self.overwrite = False
        self.dpi = 100  # movie dots per inch
        self.moviePipe = None
        self.newframe = parent
        self.persistentVars = dict()

        self.DrawWaitingPlot()

        EVT_CLOSEOP(self, self.OnCloseCP)
        EVT_REFRESHGRAPH(self, self.OnRefreshGraph)
        self.Bind(wx.EVT_SIZE, self.OnResize)

    def OnResize(self, event):
        self.OnRefreshGraph(event)
        event.Skip() # This is required to pass events up the chain.

    def setRecordStatus(self, record, fname, overwrite):
        self.movieFileName = fname
        self.overwrite = overwrite
        if (not self.recordVideo) and record:  # If record was off and changed to on
            fps = 15
            self.recordingSize = self.context.UIElement().GetSize()
            print self.recordingSize
            self.moviePipe = Popen(
                ['ffmpeg', '-f', 'rawvideo','-pix_fmt','argb','-s:v',str(self.recordingSize[0])+'x' +
                    str(self.recordingSize[1]) ,'-framerate', '24', '-i', '-', '-c:v', 'libx264', '-preset', 'veryslow', '-crf', '0','-r',
                    '24', '-y', self.movieFileName], stdin=PIPE)
            # Prevent frame resizing while recording
            self.mainframe.lockFrame()
        if self.recordVideo and not record:  # Turned off record
            # self.movieWriter.finish()
            self.mainframe.unlockFrame()
            self.moviePipe.stdin.flush()
            self.moviePipe.stdin.close()
            self.moviePipe.wait()

        self.recordVideo = record
        if record:
            self.recButton.SetBitmapLabel(self.RecOnBmp)
        else:
            self.recButton.SetBitmapLabel(self.RecOffBmp)

    def getRecordStatus(self):
        return self.recordVideo, self.movieFileName, self.overwrite

    def initializeContext(self, context):
        vsizer1 = wx.BoxSizer(orient=wx.VERTICAL)
        self.context = Contexts.context_table[context](self, self.onclick, self.onmotion, self.OnRightDown)
        # Set up GUI elements
        self.toolbar = wx.BoxSizer(orient=wx.HORIZONTAL)
        #temporarily disable toolbar
        #self.navMenu = MyCustomToolbar(self.mycanvas)
        #self.toolbar.Add(self.navMenu)
        self.toolbar.Add(self.slopeButton)
        self.toolbar.Add(self.graphOptionsButton)
        self.toolbar.Add(self.recButton)
        vsizer1.Add(self.context.UIElement(), proportion=1, flag=wx.EXPAND | wx.ALL, border=0)
        vsizer1.Add(self.toolbar, flag=wx.EXPAND | wx.ALL, border=0)
        self.SetSizerAndFit(vsizer1)

    def loadWidgets(self):
        slopebitmap = wx.Bitmap("./gui/slope.png", wx.BITMAP_TYPE_ANY)
        self.slopeButton = wx.BitmapButton(self, wx.ID_ANY, bitmap=slopebitmap,
                                           size=(42, 42))  # create slope measurement button
        opts = wx.Bitmap("./gui/options.png", wx.BITMAP_TYPE_ANY)
        self.graphOptionsButton = wx.BitmapButton(self, id=wx.ID_ANY, bitmap=opts, size=(42, 42))
        self.RecOnBmp = wx.Bitmap("./gui/rec.png", wx.BITMAP_TYPE_ANY)
        self.RecOffBmp = wx.Bitmap("./gui/recoff.png", wx.BITMAP_TYPE_ANY)
        self.recButton = wx.BitmapButton(self, id=wx.ID_ANY, bitmap=self.RecOffBmp,
                                         size=(self.RecOffBmp.GetWidth() + 10, self.RecOnBmp.GetHeight() + 10))
        self.recButton.Bind(wx.EVT_BUTTON, self.OnRecord)
        self.graphOptionsButton.Bind(wx.EVT_BUTTON, self.OnOptions)
        self.slopeButton.Bind(wx.EVT_BUTTON, self.OnMeasureButton)

    def createGraph(self):
        self.loadWidgets()
        self.initializeContext('default')

    def OnResult(self, event):
        """Show Result status."""
        if event.data is None:
            # Thread aborted (using our convention of None return)
            self.status.SetLabel('Computation aborted')
        else:
            self.currentEvent = event # Copy because event is a C++ object that gets deleted
            self.currentEvent.data._PV = self.persistentVars  # Gives the plot a simple dictionary to save persistent vars
            try:
                self.DrawPlot()
            except ValueError:
                True #Probably trying to plot something negative in an image
            # Save movie frame
            if self.recordVideo:
                #print "Writing " + str(self.currentEvent.data.simTime)
                self.movieDim = self.context.getCanvasSize()
                imdata = self.context.getARGB() # Get data byte string
                self.moviePipe.stdin.write(imdata)

    """def OnChangeGraph(self, event):
        try:
            self.currentEvent.data
            try:
                self.currentEvent._PV
                self.persistentVars = dict()
                self.currentEvent.data._PV = self.persistentVars
            except:
                True
        except:
            True """

    def DrawPlot(self):
    	if not hasattr(self.currentEvent, "data"): # Check if there is a data field set
    		return # If does not exist, return and wait for data
    	# Check if persistent vars _PV exists in data
        if not hasattr(self.currentEvent.data, "_PV"):
            self.currentEvent.data._PV = self.persistentVars # Create is if not
        else: # Copy persistent variables to object being draw
            for key in self.persistentVars.iterkeys():
                if hasattr(self.currentEvent.data, key): #Check if key is in object
                    # Copy persistent value to object
                    setattr(self.currentEvent.data, key, self.persistentVars[key])
        # Setup a drawing context of the correct type, determined by the plot to draw
        if hasattr(self.currentEvent.data, "context_type"): # Determine which context to use
            context_type_name = self.currentEvent.data.context_type # gets a string
	else:
            context_type_name = 'default' # return default class
        context_type = Contexts.context_table[context_type_name] 
        # Check if self.context is an instance of context_type.  
        # If not, create a new context
	if not isinstance(self.context,  context_type):
                newcontext = context_type(self, self.onclick, self.onmotion, self.OnRightDown)
	        self.GetSizer().Replace(self.context.UIElement(), newcontext.UIElement())
	        del self.context
	        self.context = newcontext
	        self.Layout()
	# A valid context should be in place
        self.ResetPlot()	# Reset the graph
        try:
            self.currentEvent.data.setParams(self.arbGraphParameters)  # pass paramters to plot
        except AttributeError:
            True
        self.context.plotObject(self.currentEvent.data)
        self.context.drawContext()

        if self.measuring and self.slopeMousePointer is not None:
            self.context.PlotLine(self.slopeStack[0], self.slopeMousePointer)

    def DrawWaitingPlot(self):
        self.ResetPlot()
        if not hasattr(self, 'staticImage'):
            self.staticImage = NP.random.random((50,50))
        wg = Graphs.DrawSimpleImage("Waiting for data...", self.staticImage, "Waiting for data...", extent=None)
        self.context.plotObject(wg)
        self.context.drawContext()

    def ResetPlot(self):
        self.context.resetGraph()

    def OnMeasureButton(self, event):
        if not hasattr(self.currentEvent, 'data') or self.currentEvent.data is None:
            return
        if (len(self.slopeStack) == 0 and self.measuring == True):
            self.measuring = False
            # self.slopeButton.SetValue(False)
            self.mainframe.status.SetStatusText("I await your command")
            return
        self.DrawPlot()
        self.slopeStack = []
        self.measuring = True
        # self.slopeButton.SetValue(True)
        self.mainframe.status.SetStatusText("Click the two points that define your slope")

    def OnRefreshGraph(self, event):
        self.DrawPlot()


    def OnOptions(self, event):  # Open options menu
        if self.newCP == None:  # Only allow one options window at a time
            #try:
            self.newCP = self.currentEvent.data.makeControlPanel(self)
            self.newCP.Move(event.EventObject.GetScreenPosition() )
            #except AttributeError:
            #    True

    def OnCloseCP(self, event):
        self.newCP.Hide()
        self.newCP.Destroy()
        self.newCP = None
        self.DrawPlot()  # refresh changes

    def onmotion(self, event):
        if len(self.slopeStack) == 1:
            x1 = self.slopeStack[0]
            x2 = NP.array([event.xdata, event.ydata])
            self.slopeMousePointer = x2
            self.measuring = True
            self.DrawPlot()

    def onclick(self, event):
        if event.inaxes == None or self.measuring == False:
            return
        if len(self.slopeStack) < 2:
            self.slopeStack.append(NP.array([event.xdata, event.ydata]))
            self.mainframe.status.SetStatusText(
                "First Point selected is " + str((event.xdata, event.ydata)) + ". Select next point to find slope")
        if len(self.slopeStack) == 2:
            x1 = self.slopeStack[0]
            x2 = self.slopeStack[1]
            mv = x2 - x1
            m = mv[1] / mv[0]
            self.context.PlotLine(x1, x2)
            self.mainframe.status.SetStatusText("The slope is " + str(m))
            self.slopeStack = []
            self.measuring = False
            self.slopeMousePointer = None

    # self.slopeButton.SetValue(False)

    def OnRecord(self, event):
        if spawn.find_executable('ffmpeg') is None:
            wx.MessageBox('Download and install ffmpeg to create movies.  It is free!  On Ubuntu based Linux, type: sudo apt-get install ffmpeg')
            return

        ne = RecordPanel(self)

        # Returns true if the graph is frozen (cannot switch to a new graph type)
        # For example, when recording a Velocity Plot, cannot change to a Phase plot

    def isFrozen(self):
        return self.recordVideo
