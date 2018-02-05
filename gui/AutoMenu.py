import re
import wx
import math

# A string widget
class ControlWidget:
    def __init__(self, label, panel, key, obj, updateHandler):
        self.label = wx.StaticText(panel, -1, label)
        self.data = wx.TextCtrl(panel, -1)
        self.data.SetValue(str(getattr(obj, key)))
        self.key = key
        self.obj = obj
        self.updateHandler = updateHandler

    def OnEvent(self, event):
        setattr(self.obj, self.key, event.GetString())

    def OnLostFocus(self, event):
        self.data.SetValue(str(getattr(self.obj, self.key)))        

    def Update(self):
        self.data.SetValue(str(getattr(self.obj, self.key)))

class StringWidget(ControlWidget):
    def __init__(self, label, panel, key, obj, updateHandler):
        ControlWidget.__init__(self, label+" (string)", panel, key, obj, updateHandler)

    def OnEvent(self, event):
        val = str(event.GetString())
        setattr(self.obj, self.key, val)
        self.updateHandler(self.key, val)

# A Floating point widget
class FloatWidget(ControlWidget):
    def __init__(self, label, panel, key, obj, updateHandler):
        ControlWidget.__init__(self, label+" (float)", panel, key, obj, updateHandler)

    def OnEvent(self, event):
        try:
            numval = float(event.GetString().strip())
            setattr(self.obj, self.key, numval)
            self.updateHandler(self.key, numval)
        except ValueError:
            pass

# A integer widget
class IntWidget(ControlWidget):
    def __init__(self, label, panel, key, obj, updateHandler):
        ControlWidget.__init__(self, label+" (int)", panel, key, obj, updateHandler)

    def OnEvent(self, event):
        try:
            numval = int(event.GetString().strip())
            setattr(self.obj, self.key, numval)
            self.updateHandler(self.key, numval)
        except ValueError:
            pass

def autoGenerateMenu(obj, panel, updateHandler):
    def verifyPropertyFormat(label, panel, key, obj, updateHandler):
        cond = type(getattr(obj, key)) 
        if cond is float:
            return FloatWidget(label, panel, key, obj, updateHandler)
        elif cond is int:
            return IntWidget(label, panel, key, obj, updateHandler)
        elif cond is str:
            return StringWidget(label, panel, key, obj, updateHandler)
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
            field = verifyPropertyFormat(label, panel, key, obj, updateHandler)
            if field is not None:
                properties.append(field)
    return properties

if __name__ == "__main__":
    class MainApp(wx.App):
        """Class Main App."""
        class Test:
            def __init__(self):
                self.m_IsCute = "True"
                self.m_E = 1
                self.m_Fsdf = "True"
                self.m_Esdsd = 1.0

                self.m_myField = 1
                self.c_MayField = 1

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

            a = MainApp.Test()
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
                c.data.Bind(wx.EVT_KILL_FOCUS, c.OnLostFocus)

            # Add Lower Padding
            lowerPadding = 10
            left.Add((0,lowerPadding))
            right.Add((0,lowerPadding))

            # Fit panel and frames to controls
            panel.SetSizer(vbox)
            panel.Layout()
            panel.Fit()
            self.frame.Fit()

    app = MainApp(0)
    app.MainLoop()
