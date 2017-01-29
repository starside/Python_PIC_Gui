"""
This class is intended to be inherited.  It simply communicates data from a frame to a loader object
"""
import pickle
import wx

"""
This class is just reuable functions to handle common taks
"""


class DefaultsCommLink:
    def saveAsDefault(self, loader, windowname):
        wr = self.GetRect()
        windowrect = [i for i in wr]
        md = {"WindowRect": windowrect}
        loader.updateLoader(windowname, md)

    def readDefault(self, loader, windowname):
        try:
            md = loader.defaultDictionary[windowname]
        except KeyError:
            print "Could not load defaults " + str(windowname)
            return  # could not load defaults
        wr = wx.Rect(md["WindowRect"][0], md["WindowRect"][1], md["WindowRect"][2], md["WindowRect"][3])
        self.SetRect(wr)


"""
This class handles loading the defaults from file
"""


class DefaultLoader:
    default_file = "new_beps1.defaults"

    def __init__(self, filename):
        self.file = filename
        self.defaultDictionary = {}

    def updateLoader(self, windowname, theDict):
        self.defaultDictionary[windowname] = theDict
        print self.defaultDictionary

    def saveToFile(self):
        with open(DefaultLoader.default_file, "w") as fh:
            pickle.dump(self.defaultDictionary, fh)

    def loadFromFile(self):
        try:
            with open(DefaultLoader.default_file, "r") as fh:
                self.defaultDictionary = pickle.load(fh)
        except IOError:
            print "Could not open the defaults file.  I suggest you make one, by using the Layout/Save Defaults Menu"
