from collections import deque


class GraphStack:
    def __init__(self, ss, name, desc, callback=None):
        self.description = desc
        self.name = name
        self.listenerQ = []  # List of registered listeners.  Unlimited size
        self.stack = deque()  # internal stack
        self.stackSize = ss  # set stack size
        self.callback = callback

    def OnResult(self, event):  # event handler
        self.stack.append(event)
        if len(self.stack) > self.stackSize:
            self.stack.popleft()
        self.broadcast(event)

    def AddListener(self, obj, pos=1):  # Pos currently does nothing, but I may change that
        self.listenerQ.append([pos, obj])
        if hasattr(self.callback, "GraphStackChanged"):
            self.callback.GraphStackChanged(self.countListeners(), self.name)

    def RemoveListener(self, obj):
        rem = None
        for i in self.listenerQ:
            if i[1] == obj:
                rem = i
        if rem != None:
            self.listenerQ.remove(rem)
            if hasattr(self.callback, "GraphStackChanged"):
                self.callback.GraphStackChanged(self.countListeners(), self.name)
            return True
        return False

    def broadcast(self, event):
        for l in self.listenerQ:  # dispatch result signal to all listener objects
            l[1].OnResult(event)

    def getRecent(self):
        if len(self.stack) > 0:
            return self.stack[-1]
        else:
            return None

    def countListeners(self):
        return len(self.listenerQ)


class DispList:
    def __init__(self):
        self.dl = []

    def __iter__(self):
        return iter(self.dl)


class Dispatcher:
    def initStack(self):
        self.dispatchers = []

    def OnResult(self, event):
        for d in self.dispatchers:  # Route events to proper stack
            if d.name == event.name:
                d.OnResult(event)
