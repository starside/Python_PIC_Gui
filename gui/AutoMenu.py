import re
import wx
import math

class Test:
    def __init__(self):
        self.m_MiMyField = "Johsd"
        self.m_EmmyIsCute = "True"
        self.m_E = "True"
        self.m_Fsdf = "True"
        self.m_Esdsd = "True"

        self.m_myField = 1
        self.c_MMyField = 1

# A Floating point widget
class FloatWidget:
    def __init__(self, property, label, panel, key, obj):
        self.property = property
        print("Floating Point"+str(property)+label)

# A integer widget
class IntWidget:
    def __init__(self, property, label, panel, key, obj):
        self.property = property
        self.label = wx.StaticText(panel, -1, label+"(int)")
        self.data = wx.TextCtrl(panel, -1)
        self.data.SetValue(self.property)
        self.key = key
        self.obj = obj

    def OnEvent(self, event):
        setattr(self.obj, self.key, event.GetString())

    def LostFocus(self, event):
        pass

# A string widget
class StringWidget:
    def __init__(self, property, label, panel, key, obj):
        self.property = property
        self.label = wx.StaticText(panel, -1, label+"(string)")
        self.data = wx.TextCtrl(panel, -1)
        self.data.SetValue(self.property)
        self.key = key
        self.obj = obj

    def OnEvent(self, event):
        setattr(self.obj, self.key, event.GetString())

    def LostFocus(self, event):
        pass

def autoGenerateMenu(obj, panel):
    def verifyPropertyFormat(property, label, panel, key, obj):
        cond = type(property) 
        if cond is float:
            return FloatWidget(property, label, panel, key, obj)
        elif cond is int:
            return IntWidget(property, label, panel, key, obj)
        elif cond is str:
            return StringWidget(property, label, panel, key, obj)
        return None # No matching widget found


    properties = []
    # Match m_ Followed by capitalized alphanumerics
    p = re.compile('^m_([A-Z]\w*)')
    # Search Keys
    for key in vars(obj).keys():
        res = p.match(key)
        if res is not None:
            propName = res.group(1) #  Get property name
            s = re.compile(r'([A-Z][a-z0-9]*)') #Split based on capitalization
            words = [x for x in s.split(propName) if len(x) > 0] # Remove empty strings
            label = " ".join(words) #Convert name to label
            property = getattr(obj, key) #read property from object
            field = verifyPropertyFormat(property, label, panel, key, obj)
            if field is not None:
                properties.append(field)
    return properties


class MainApp(wx.App):
    """Class Main App."""

    def __init__(self, arg):
        wx.App.__init__(self, arg)
        self.frame = wx.Frame(None, -1, "Josh")
        self.InitUI()
        self.frame.Show(True)

    def InitUI(self):
        panel = wx.Panel(self.frame)

        vbox = wx.BoxSizer(wx.HORIZONTAL)

        left = wx.BoxSizer(wx.VERTICAL)
        right = wx.BoxSizer(wx.VERTICAL)

        vbox.Add(left, 1, flag=wx.EXPAND)
        vbox.Add(right, 1, flag=wx.EXPAND)

        a = Test()
        controls = autoGenerateMenu(a, panel)

        deltaTop = 0
        for c in controls:
            # Calculate label padding
            _, labelY = c.label.GetClientSize()
            _, dataY = c.data.GetClientSize()
            padding = (dataY - labelY)/2.0
            topPad = math.floor(padding) + deltaTop
            bottomPad = math.ceil(padding)
            # Add default spacing
            left.Add((0,7))
            # Add top padding plus accumulated offset
            left.Add((0,topPad))
            left.Add(c.label, flag=wx.ALIGN_RIGHT)
            left.Add((0, bottomPad))
            # Update error offset for top padding
            deltaTop = dataY - (topPad + labelY + bottomPad - deltaTop)

            # Build the right column, add the field
            right.Add((0,7))
            right.Add(c.data)
            c.data.Bind(wx.EVT_TEXT, c.OnEvent)
            c.data.Bind(wx.EVT_KILL_FOCUS, c.LostFocus)

        panel.SetSizer(vbox)
        panel.Layout()


if __name__ == "__main__":
    app = MainApp(0)
    app.MainLoop()
