"""
Copyright 1999 Illinois Institute of Technology

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL ILLINOIS INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Illinois Institute
of Technology shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from Illinois Institute of Technology.
"""

import sys
import os
from os.path import split
from musclex import __version__
from .pyqt_utils import *
from ..ui.EquatorWindow import EquatorWindow

class EQStartWindow(QMainWindow):
    """
    A class for start-up window or main window. Now, this is used for keep all EquatorWindow objects in a list
    """
    def __init__(self):
        print("EQSTART init") #NICKA DEBUG
        super().__init__()
        print("EQSTART AFTER SUPER INIT") #NICKA DEBUG
        self.dir_path = ""
        self.setWindowTitle("Bio-Muscle v."+__version__)
        self.windowList = [] # use this list to keep EquatorWindow objects to prevent python delete it
        
        #self.browseFile() # start program by browse a file
        print("EQSTART INIT ABOUT TO CALL RUNBIOMUSCLE") #NICKA DEBUG
        self.runBioMuscle()

    def childWindowClosed(self, childwin):
        """
        Remove child window from list
        :param childwin: EquatorWindow object
        """
        print("EQSTART CHILD WINDOW CLOSED") #NICKA DEBUG
        if childwin in self.windowList:
            print("WINDOWLIST REMOVE") #NICKA DEBUG
            self.windowList.remove(childwin)

        # If window list is empty, exit the program
        if len(self.windowList) == 0:
            print("SELF.CLOSE")
            self.close()

    def runBioMuscle(self):
        """
        Create a EquatorWindow object and launch the window
        :param filename: input filename (str)
        :return:
        """
        print("runBIOMUSCLE") #NICKA DBEUG
        try:
            print("Tryblock") #NICKA DEBUG

            newWindow = EquatorWindow(self)

        except Exception as e:
            print("UH OH- EXCEPITON") #NICKA DEBUG
            infMsg = QMessageBox()
            infMsg.setText("Error")
            infMsg.setInformativeText(str(e))
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
        print("Appending new window") #NICKA DEBUG
        self.windowList.append(newWindow)
        print("Hiding self") #NICKA DEBUG
        self.hide()
