""" Graphical contexts """
from collections import namedtuple
import wx

class Context:
    def __init__(self, home):
        raise NotImplementedError

    def UIElement(self):
        raise NotImplementedError

    def contextType(self):
        raise NotImplementedError

    def resetGraph(self):
        raise NotImplementedError

    def plotObject(self, obj):
        raise NotImplementedError

    def registerMouseCallbacks(self, click, motion, rightclick):
        self.click_handler = click
        self.motion_handler = motion
        self.rightclick_handler = rightclick

    def drawContext(self):
        raise NotImplementedError

    def getCanvasSize(self):
        raise NotImplementedError

    def getARGB(self):
        """ Returns byte string of ARGB formatted data """
        raise NotImplementedError

    def PlotLine(self, x1, x2):
        """ Draw a line on plot points (in axes space) between points x1 and x2
            where x1 and x2 are tuples of the format (x, y) """
        raise NotImplementedError

class NullContext(Context):
    def __init__(self, home, onclick, onmotion, onrightclick):
        self.ui = wx.Button(home, label="Left or right click here to select a plot")
        # Setup mouse callbacks
        self.registerMouseCallbacks(onclick, onmotion, onrightclick)
        self.UIElement().Bind(wx.EVT_LEFT_DOWN, onrightclick)
	self.UIElement().Bind(wx.EVT_RIGHT_DOWN, onrightclick)

    def UIElement(self):
        return self.ui

    def contextType(self):
        return 'null'

    def resetGraph(self):
        pass

    def plotObject(self, obj):
        pass

    def drawContext(self):
        pass

    def getCanvasSize(self):
        s = self.UIElement().GetSize()
        return (s.width, s.height)

    def getARGB(self):
        """ Returns byte string of ARGB formatted data """
        w,h = self.getCanvasSize()
        return w*h*4*"\x00"

    def PlotLine(self, x1, x2):
        """ Draw a line on plot points (in axes space) between points x1 and x2
            where x1 and x2 are tuples of the format (x, y) """
        pass

    def __del__(self):
        print "del"

class MatplotLibContext(Context):
    def __init__(self, home, onclick, onmotion, onrightclick):
        """ Load modules """
        import matplotlib.image as mpimg
        import matplotlib
        import matplotlib.cm as cm
        from mpl_toolkits.mplot3d import Axes3D
        from matplotlib.colors import LogNorm
        from matplotlib.figure import Figure
        from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
        from matplotlib.backends.backend_wx import NavigationToolbar2Wx
        from mpl_toolkits.axes_grid.anchored_artists import AnchoredText

        # Actual code
        self.figure = matplotlib.figure.Figure()
        self.axes = self.figure.add_subplot(111)
        self.home = home
        self.mycanvas = FigureCanvas(self.home, -1, self.figure)
        self.mycanvas.SetSize((100, 100))

        # Setup mouse callbacks
        self.registerMouseCallbacks(onclick, onmotion, onrightclick)
        self.mycanvas.mpl_connect('button_press_event', self.onClick)
        self.mycanvas.mpl_connect('motion_notify_event', self.onMotion)
	self.UIElement().Bind(wx.EVT_RIGHT_DOWN, onrightclick)

    def onClick(self, event):
        if self.click_handler is None:
            return
        self.click_handler(event)

    def onMotion(self, event):
        if self.motion_handler is None:
            return
        self.motion_handler(event)

    def registerMouseCallbacks(self, click, motion, rightclick):
        self.click_handler = click
        self.motion_handler = motion
        self.rightclick_handler = rightclick

    def contextType(self):
        return "matplotlib"

    def resetGraph(self):
        self.figure.delaxes(self.axes)
        self.figure.clf()
        self.axes = self.figure.add_subplot(111)

    def plotObject(self, obj):
        rax = obj.drawPlot(self.figure, self.axes)
        if rax != None:  # This allows the graph to reconfigure its own axes, instead of allowing the panel to handle it
            self.axes = rax

    def drawContext(self):
        self.mycanvas.draw()

    def UIElement(self):
        return self.mycanvas

    def getCanvasSize(self):
        return self.mycanvas.get_width_height()

    def getARGB(self):
        return self.mycanvas.tostring_argb()

    def PlotLine(self, x1, x2):
        self.axes.plot([x1[0], x2[0]], [x1[1], x2[1]])
        self.mycanvas.draw()

    def __del__(self):
        print("del")

class MathGlContext(Context):
    def __init__(self, home, onclick, onmotion, onrightclick):
        """ Load modules """
        import mathgl
        self.__lmgl = mathgl
        # Actual code
        self.ui = wx.Window(home, size=(200,200)) # Create UI Area
        self.resetGraph()
        self.ui.Show(True)
        # Setup mouse callbacks
        self.registerMouseCallbacks(onclick, onmotion, onrightclick)
	self.UIElement().Bind(wx.EVT_RIGHT_DOWN, onrightclick)
        self.UIElement().Bind(wx.EVT_PAINT, self._OnPaint)

    def _OnPaint(self, event):
        self._resize()
        numbytes = len(self.bits)
        self.graph.GetRGB(self.bits, numbytes) # This is the slowest call
        img = wx.Image(self.size.width, self.size.height, self.bits)
        dc = wx.PaintDC(self.ui)
        dc.DrawBitmap(wx.Bitmap(img), 0, 0) # Second slowest call

    def onClick(self, event):
        if self.click_handler is None:
            return
        self.click_handler(event)

    def onMotion(self, event):
        if self.motion_handler is None:
            return
        self.motion_handler(event)

    def registerMouseCallbacks(self, click, motion, rightclick):
        self.click_handler = click
        self.motion_handler = motion
        self.rightclick_handler = rightclick

    def contextType(self):
        return "mathgl"

    def resetGraph(self):
        size = self.ui.GetClientSize() # Get initial size
        self.size = size
        self.ui.SetBackgroundColour("BLACK")
        self.graph = self.__lmgl.mglGraph(0, size.width, size.height) # Create initial plot
        # Create image buffer
        bpp = 3 # bytes per pixel
        numpix = self.size.width*self.size.height
        numbytes = bpp*numpix
        self.bits = '\t'.expandtabs(numbytes)

    def _resize(self):
        w1, h1 = self.size
        size = self.ui.GetClientSize()
        w2, h2 = size
        if w1 != w2 or h1 != h2:
            self.resetGraph()
            self.size = size

    def plotObject(self, obj):
        obj.drawPlot(self.graph)
    
    def drawContext(self):
        self.ui.Refresh()
        self.ui.Update()

    def UIElement(self):
        return self.ui

    def getCanvasSize(self):
        s = self.UIElement().GetSize()
        return (s.width, s.height)

    def getARGB(self):
        #return self.mycanvas.tostring_argb()
        pass

    def PlotLine(self, x1, x2):
        #self.axes.plot([x1[0], x2[0]], [x1[1], x2[1]])
        #self.mycanvas.draw()
        pass

    def __del__(self):
        print("del")

#List of all availible contexts
context_table = {'default':MatplotLibContext, 'matplotlib':MatplotLibContext, 'null':NullContext, \
                'mathgl':MathGlContext}

