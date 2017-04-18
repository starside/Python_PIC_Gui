import sys
import time

#This file, main.py must be able to find the gui files in ./gui
sys.path.append('./gui')
from ProceduralInterface import *
from GraphicsInterface import GraphicsInterface

#Define a main function
def main(*args):
    # init GUI
    pc = PlasmaContext(*args)  # Create GUI
    pc.showGraphs(True)  # enable graphics.  Setting to false will disable graphics
    pc.clearGraphList()  # remove all default graph options
    pc.newFrame("Layout4", []) #Create an empty frame with 4 plots layed out
    while True:
        pc.getEvents(None)  #Process events from GUI
        time.sleep(0.01)    #Sleep so we dont use all CPU idling.  This is not required in most programs

#Run the main function.  For technical reasons, you cannot call main() directly, and instead should use runMain.  *
PlasmaContext.runMain(main)


#*For the technically inclined, the reason for using runMain to call main i explained.
#The GUI requires two processes to run.  One process is the loop that handles drawing
#the GUI, responding to events such as button clicks, drawing plots, etc.  This
#process is controlled by wxPython.

#The second process is the simulation process, which is defined by main. When you
#invoke main.py (which I call the host process) will run the GUI loop.  It also
#spawns a child process, which runs the simulation.  For compatibility with Mac,
#the host process cannot run in the child process, so to keep your programs cross
#platform, the GUI library forces your code to be run with PlasmaContext.runMain.
#In other words, runMain forks a new process and runs your simulation code.