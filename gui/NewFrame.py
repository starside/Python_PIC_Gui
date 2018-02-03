import wx
import wx.stc as stc
from collections import deque
import sys

from defaults import *
from GraphContainer import *


# I need an event to remove listeners when the window gets closed!
class NewFrame(wx.Frame, DefaultsCommLink):
    def __init__(self, parent, loader, dispatch, layout=None, defaults=[]):
        """Create the MainFrame."""
        wx.Frame.__init__(self, parent, -1, 'PIC Interface.  J. Kelly & V. Decyk', style=wx.DEFAULT_FRAME_STYLE)
        self.loader = loader
        self.layoutName = ""
        self.InitData()
        self.InitMenu()
        self.status = self.CreateStatusBar()
        self.stf = dispatch
        self.Bind(wx.EVT_CLOSE, self.OnQuit)
        self.mainframe = parent

        self.SetPosition((200, 200))
        if layout == None:
            self.OnLayout1(None)  # default layout
        else:
            if hasattr(self, "On" + layout) and layout.startswith("Layout"):  # We have the specified Layout
                cl = getattr(self, "On" + layout)
                cl(None)  # create the layout
                # Now populate with defaults
                try:
                    for i, area in enumerate(self.displayAreas):
                        area.setGraphByName(defaults[i])
                except:
                    sys.stderr.write("Not all graphs specified.  Not an issue\n")
            else:  # Fail if the specified layout does not exist
                sys.stderr.write(
                    "The layout you specified " + layout + " is not defined in NewFrame.py.  Expect a method name starting with OnLayout\n")
                exit(0)

        self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
        self.Bind(wx.EVT_MOVE, self.OnMove)
        self.mainframe.activeFrame = self
        self.Show()

    def InitData(self):
        self.displayAreas = deque()

    def PruneDisplays(self, num):  # Prune displays down to number num
        while len(self.displayAreas) > num:
            toDie = self.displayAreas.pop()
            toDie.Hide()
            for d in self.stf.dispatchers:
                d.RemoveListener(toDie)
            toDie.Destroy()

    def _AddDisplay(self):
        toLive = GraphContainer(self, self.stf.dispatchers)
        return toLive

    def RequestDisplays(self, num):
        if num > len(self.displayAreas):
            nd = num - len(self.displayAreas)
            for i in range(nd):  # create nd new displays
                self.displayAreas.append(self._AddDisplay())

    def InitMenu(self):
        menubar = wx.MenuBar()
        fileMenu = wx.Menu()
        layoutMenu = wx.Menu()
        viewMenu = wx.Menu()
        fitem = fileMenu.Append(wx.ID_EXIT, 'Close', 'Close This Window')
        latem1 = layoutMenu.Append(wx.NewId(), '1 Graph')
        latem2h = layoutMenu.Append(wx.NewId(), '2 Graphs - Horizontal')
        latem2v = layoutMenu.Append(wx.NewId(), '2 Graphs - Vertical')
        latem3 = layoutMenu.Append(wx.NewId(), '3 Graphs')
        latem4 = layoutMenu.Append(wx.NewId(), '4 Graphs')
        self.layoutItems = [latem1, latem2h, latem2v, latem3, latem4]  # Need this list to enable, disable the layouts
        viewcbar = viewMenu.Append(wx.NewId(), "Locate Control Bar")
        hideba = viewMenu.Append(wx.NewId(), "Hide/Show Nav bars")
        savedef = viewMenu.Append(wx.NewId(), "Save Window Size/Position as default")
        menubar.Append(fileMenu, '&File')
        menubar.Append(layoutMenu, 'Layout')
        menubar.Append(viewMenu, 'View')
        self.SetMenuBar(menubar)

        self.Bind(wx.EVT_MENU, self.OnQuit, fitem)
        self.Bind(wx.EVT_MENU, self.OnLayout1, latem1)
        menubar.Bind(wx.EVT_UPDATE_UI, self.OnLayoutMenuShow)

        self.Bind(wx.EVT_MENU, self.OnLayout2h, latem2h)
        self.Bind(wx.EVT_MENU, self.OnLayout2v, latem2v)
        self.Bind(wx.EVT_MENU, self.OnLayout3, latem3)
        self.Bind(wx.EVT_MENU, self.OnLayout4, latem4)
        self.Bind(wx.EVT_MENU, self.ToggleNav, hideba)
        self.Bind(wx.EVT_MENU, self.SaveDefault, savedef)
        self.Bind(wx.EVT_MENU, self.LocateCBar, viewcbar)

    def LocateCBar(self, event):
        af = self.mainframe.activeFrame
        (x, y, w, h) = af.GetScreenRect()
        (mx, my, mw, mh) = self.mainframe.GetScreenRect()
        self.mainframe.Move(wx.Point(x + w - mw, y))

    def enableLayoutMenu(self, state):
        for lo in self.layoutItems:
            lo.Enable(state)

    def OnLayoutMenuShow(self, event):
        freezeLayout = False
        for d in self.displayAreas:
            freezeLayout = freezeLayout or d.isFrozen()
        for e in self.layoutItems:
            e.Enable(not freezeLayout)

    def SaveDefault(self, event):
        self.saveAsDefault(self.loader, "NewFrame" + self.layoutName)
        self.loader.saveToFile()

    def OnFocus(self, event):
        wx.CallAfter(self.InitMenu)
        self.mainframe.activeFrame = self
        self.OnMove(None)

    def OnMove(self, event):
        border = 10
        af = self.mainframe.activeFrame
        if not self.mainframe.rpanel.pin.GetValue():
            (x, y, w, h) = af.GetScreenRect()
            self.mainframe.Move(wx.Point(x + w + border, y))

    def OnQuit(self, event):
        if self.mainframe is not None:  # remove self from windowList
            try:
                self.mainframe.windowList.remove(self)
            except ValueError:
                print "Could not remove frame from window list.  This should never happen!"
                exit(0)

        self.PruneDisplays(0)
        self.Hide()

    def _resetMinSize(self):
        size = self.GetSize()
        self.SetMinSize((size[0] / 2, size[1] / 2))

    def _GoodSize(self):
        self._resetMinSize()
        self.readDefault(self.loader, "NewFrame" + self.layoutName)

    def OnLayout1(self, event):
        self.layoutName = "Layout1"
        self.PruneDisplays(1)
        self.RequestDisplays(1)
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        for e in self.displayAreas:
            self.sizer.Add(e, flag=wx.ALL | wx.EXPAND, proportion=1)
        self.SetSizerAndFit(self.sizer)
        self.Layout()
        self._GoodSize()

    def OnLayout2h(self, event):
        self.layoutName = "Layout2h"
        self.PruneDisplays(2)
        self.RequestDisplays(2)
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        for e in self.displayAreas:
            self.sizer.Add(e, flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
        self.SetSizerAndFit(self.sizer)
        self.Layout()
        self._GoodSize()

    def OnLayout2v(self, event):
        self.layoutName = "Layout2v"
        self.PruneDisplays(2)
        self.RequestDisplays(2)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        for e in self.displayAreas:
            self.sizer.Add(e, flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
        self.SetSizerAndFit(self.sizer)
        self.Layout()
        self._GoodSize()

    def ToggleNav(self, event):
        for d in self.displayAreas:
            toggle = True
            if d.toolbar.IsShown(d.navMenu):
                toggle = False
            d.toolbar.ShowItems(toggle)
        self.Layout()

    def OnLayout3(self, event):
        self.layoutName = "Layout3"
        self.PruneDisplays(3)
        self.RequestDisplays(3)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        topRow = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(topRow, flag=wx.ALL | wx.EXPAND, proportion=1)
        self.sizer.Add(self.displayAreas[2], flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
        topRow.Add(self.displayAreas[0], flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
        topRow.Add(self.displayAreas[1], flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)

        self.SetSizerAndFit(self.sizer)
        self.Layout()
        self._GoodSize()

    def OnLayout4(self, event):
        self.layoutName = "Layout4"
        self.PruneDisplays(4)
        self.RequestDisplays(4)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        topRow = wx.BoxSizer(wx.HORIZONTAL)
        botRow = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(topRow, flag=wx.ALL | wx.EXPAND, proportion=1)
        self.sizer.Add(botRow, flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
        for i in range(2):
            topRow.Add(self.displayAreas[i], flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)
            botRow.Add(self.displayAreas[i + 2], flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER, proportion=1)

        self.SetSizerAndFit(self.sizer)
        self.Layout()
        self._GoodSize()

    def lockFrame(self):
        if (not hasattr(self, "sizeLocked")):
            self.sizeLocked = 0
        if self.sizeLocked == 0:
            self.oldSize = (self.GetMinSize(), self.GetMaxSize())
            cps = self.GetSize()
            self.SetMinSize(cps)
            self.SetMaxSize(cps)
        self.sizeLocked += 1  # Add a new lock

    def unlockFrame(self):
        if hasattr(self, "sizeLocked"):
            if self.sizeLocked > 0:
                self.sizeLocked += -1
            if self.sizeLocked <= 0:
                self.SetMinSize(self.oldSize[0])
                self.SetMaxSize(self.oldSize[1])
