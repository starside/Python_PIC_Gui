#For GUI to sim
class ResetSignal:
    def __init__(self):
        self.signame = "RESET"


class ExitSignal:
    def __init__(self):
        self.signame = "EXIT"


class VarChangeSignal:
    def __init__(self):
        self.signame = "VARCHANGE"
        self.var = dict()


###For outgoing to GUI

class SyncMode:
    def __init__(self):
        self.cmd = "ASYNC"
        self.async = False


class OpenFrame:
    def __init__(self):
        self.signame = "OPENFRAME"
        self.layout = 1
        self.defaults = []

class NewDynamicVariable:
    def __init__(self):
        self.signame = "NEWDYNAMICVAR"
        self.varname = ""


class SetFrameTime:
    def __init__(self, time):
        self.signame = "SETTIME"
        self.time = time


class SimData:
    """
    Class for conveying information to the simulation.  data is expected to be a dictionary
    """
    def __init__(self, data):
        self.signame = "SIMDATA"
        self.data = data


# This event manipulates GraphStack.  By default, it will clear the dispatcher list.
# You can also add a dispatcher by dynamically tacking on a codename and desc attribute
# where GraphStack(3,codename,desc)
class ClearGraphStack:
    def __init__(self):
        self.signame = "CLEARGRAPHSTACK"
