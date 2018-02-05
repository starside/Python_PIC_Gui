import wx
import wx.stc as stc

from LeftPanel import *
from Events import *


class GraphContainer(LeftPanel):
    def __init__(self, parent, dlist):
        self.parent = parent
        self.centralDispatcher = dlist
        # wx.Panel.__init__(self, parent, -1, wx.DefaultPosition, wx.DefaultSize, style=wx.SUNKEN_BORDER)
        LeftPanel.__init__(self, parent)
        self._mouseDownFlag = 0
        self.mycanvas.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
        self.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
        self.SetMinSize(wx.Size(100, 100))

    def OnRightDown(self, event):
        menu = wx.Menu()
        md = {"root": menu}
        if self.isFrozen():  # If we are currently recording video, do not allow chaning of movies
            ti = menu.Append(1, "Stop recording before changing graph type.")
            ti.Enable(False)
        else:  # Allow graph to change
            for (i, g) in enumerate(self.centralDispatcher):
                path = g.description.split("/")  # Allow 1 level of folders in menu
                if len(path) == 1:
                    ti = menu.Append(i + 1, g.description)
                    self.Bind(wx.EVT_MENU, self.PopupHandler, ti)
                elif len(path) == 2:
                    folder = path[0]
                    if not md.has_key(folder):
                        md[folder] = wx.Menu()
                        menu.AppendMenu(wx.ID_ANY, folder, md[folder])
                    ti = md[folder].Append(i + 1, path[1])
                    self.Bind(wx.EVT_MENU, self.PopupHandler, ti)
                else:
                    sys.stderr.write("Nested Folders are not currently supported\n")
                    exit(0)
        self.PopupMenu(menu, (event.GetX(), event.GetY()))
        menu.Destroy()

    def PopupHandler(self, event):
        self.resetGraph()
        self.persistentVars = dict() # Reset persistent variable container
        wasRemoved = False
        for g in self.centralDispatcher: #Remove self from central dispatch.  Clean up the old cruft
            wasRemoved = wasRemoved or g.RemoveListener(self)
        if wasRemoved:
            self.GraphChanged()
        self.centralDispatcher[event.GetId() - 1].AddListener(self) # Add self to central dispatch
        re = self.centralDispatcher[event.GetId() - 1].getRecent()
        if re != None:
            self.currentEvent = re
            self.DrawPlot()

    def GraphChanged(self):
        if hasattr(self,"newCP") and self.newCP is not None:
            if hasattr(self.newCP, "OnChangePlots"):
                self.newCP.OnChangePlots()

    def setGraphByName(self, name):  # set the graph to display by name
        self.movieFileName = "moviename.mp4"
        for g in self.centralDispatcher:
            g.RemoveListener(self)
        for g in self.centralDispatcher:
            if g.name == name:
                g.AddListener(self)
                re = g.getRecent()
                if re != None:
                    self.currentEvent = re
                    self.DrawPlot()
