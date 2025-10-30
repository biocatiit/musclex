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
from enum import Flag, auto
import cv2
from PySide6.QtCore import Qt, Slot, Signal
from PySide6.QtWidgets import (QApplication,
                               QWidget,
                               QCheckBox,
                               QMessageBox,
                               QVBoxLayout)

from .ui_widget import UIWidget


class DoubleZoomWidgetState(Flag):
    """
    DoubleZoom state management - simplified to 3 core states.
    
    State transition flow:
    IDLE → (click main image) → WAITING_ZOOM_CLICK → (click zoom window) → COMPLETED → (reset) → IDLE
    
    State descriptions:
    - IDLE: Idle state, ready to accept new clicks
    - WAITING_ZOOM_CLICK: Main image clicked, waiting for user to click zoom window
    - COMPLETED: Zoom window clicked, precise coordinates available
    """
    IDLE = auto()               # Idle state
    WAITING_ZOOM_CLICK = auto() # Main image clicked, waiting for zoom window click
    COMPLETED = auto()          # Zoom window clicked, coordinates available


class DoubleZoomWidget(UIWidget):
    def __init__(self,
        imageAxes=None,
        parent=None,
        dontShowMessage=False):
        super().__init__(imageAxes)

        self.state = DoubleZoomWidgetState.IDLE

        self.parent = parent

        self.doubleZoomAxes = None
        self.doubleZoomImage = None  # Store the image object to update it later
        self.doubleZoomCheckbox = QCheckBox("Double Zoom")
        self.layout = QVBoxLayout(self)
        self.layout.addWidget(self.doubleZoomCheckbox)

        self.doubleZoomCheckbox.checkStateChanged.connect(self.handleDoubleZoomCheckedEvent)

        # Mouse click point in main image
        self.mainImagePoint = (0, 0)
        # Mouse click point in double zoom image
        self.doubleZoomPoint = (0, 0)
        self.dontShowAgainDoubleZoomMessageResult = dontShowMessage

        self.set_ready()

    def set_state(self, state):
        self.state = state

    def is_enabled(self):
        """
        Check if DoubleZoom is currently enabled (running).
        This is an alias for is_running() for backward compatibility.
        
        Returns:
            bool: True if DoubleZoom is enabled
        """
        return self.is_running()

    def set_running(self):
        """
        Enable DoubleZoom - create zoom window, content updates dynamically on mouse move.
        
        Does not depend on center, avoiding crashes due to missing data.
        Zoom window content is automatically displayed when mouse moves over image.
        """
        self.doubleZoomCheckbox.setChecked(True)
        
        # Create zoom window subplot
        if self.doubleZoomAxes is None:
            self.doubleZoomAxes = self.imageFigure.add_subplot(333)
            self.doubleZoomAxes.set_aspect('equal', adjustable="box")
            self.doubleZoomAxes.axes.xaxis.set_visible(False)
            self.doubleZoomAxes.axes.yaxis.set_visible(False)
            
            # Display hint text to guide user
            self.doubleZoomAxes.text(
                0.5, 0.5, 
                'Move mouse\nover image',
                ha='center', 
                va='center',
                transform=self.doubleZoomAxes.transAxes,
                fontsize=10, 
                color='gray',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.3)
            )
        
        # Reset image object for new session
        self.doubleZoomImage = None
        
        self.imageCanvas.draw_idle()
        super().set_running()

    def set_ready(self):
        self.doubleZoomCheckbox.setChecked(False)

        self.remove_image_lines(labels=["DoubleZoom Blue Dot"])

        if self.doubleZoomAxes is not None:
            self.imageFigure.delaxes(self.doubleZoomAxes)
            self.doubleZoomAxes = None
        
        # Reset image object reference
        self.doubleZoomImage = None
        
        self.imageCanvas.draw_idle()

        super().set_ready()


    def handleDoubleZoomCheckedEvent(self, doubleZoomCheckboxState):
        if self.parent is None or self.parent.quadFold is None or self.parent.quadFold.orig_img is None:
            return

        if doubleZoomCheckboxState == Qt.CheckState.Checked:
            self.set_running()
        elif doubleZoomCheckboxState == Qt.CheckState.Unchecked:
            self.set_ready()

    def handle_mouse_button_press_event(self, mouse_event):
        """Legacy method - kept for compatibility"""
        pass
    
    def handle_click(self, mouse_event) -> str:
        """
        Handle click event and return status for event pipeline integration.
        
        This method is designed to work as a coordinate precision layer that
        sits before the tool system. It intercepts clicks, shows a zoom window,
        and provides precise coordinates.
        
        Args:
            mouse_event: The matplotlib mouse event
        
        Returns:
            'idle': DoubleZoom not enabled or event not relevant
            'waiting': Main image clicked, waiting for zoom window click
            'completed': Zoom window clicked, precise coordinates available
        
        Usage in event pipeline:
            if self.doubleZoom.is_enabled():
                status = self.doubleZoom.handle_click(event)
                if status == 'waiting':
                    return  # Block further processing
                elif status == 'completed':
                    x, y = self.doubleZoom.get_precise_coords()
                    event.xdata, event.ydata = x, y  # Modify event coords
        """
        if not self.is_enabled():
            return 'idle'
        
        # Process the click using the existing release handler
        self.handle_mouse_button_release_event(mouse_event)
        
        # Return status based on simplified state
        if self.state == DoubleZoomWidgetState.WAITING_ZOOM_CLICK:
            return 'waiting'
        elif self.state == DoubleZoomWidgetState.COMPLETED:
            return 'completed'
        else:
            return 'idle'

    def handle_mouse_move_event(self, mouse_event):
        """
        Handle mouse move event, update zoom window content and cursor indicators.
        """
        if not self.is_running():
            return

        if self.parent.quadFold is None or self.parent.quadFold.orig_img is None:
            return

        x = mouse_event.xdata
        y = mouse_event.ydata

        img = self.parent.quadFold.orig_img

        if mouse_event.inaxes == self.imageAxes:
            # If main image clicked, waiting for zoom window click, freeze zoom window
            # Do NOT update zoom window content, just draw cursor
            if self.state == DoubleZoomWidgetState.WAITING_ZOOM_CLICK:
                # Only draw blue dot on main image, do not update zoom window
                self.drawBlueDot(x, y, self.imageAxes)
                self.imageCanvas.draw_idle()
                return

            # In normal state, draw cursor and update zoom window content in real-time
            self.drawBlueDot(x, y, self.imageAxes)
            self.drawDoubleZoomImage(x, y, img)
            self.imageCanvas.draw_idle()

        elif mouse_event.inaxes == self.doubleZoomAxes:
            # Draw cursor location in zoom using red cross lines.
            self.drawRedDot(x, y, self.doubleZoomAxes)
            self.imageCanvas.draw_idle()

    def handle_mouse_button_release_event(self, mouse_event):
        """
        Handle mouse button release event, record click position and update state.
        """
        if not self.is_running():
            return

        x = mouse_event.xdata
        y = mouse_event.ydata

        if mouse_event.inaxes == self.imageAxes:
            # User clicked main image
            if not self.dontShowAgainDoubleZoomMessageResult:
                self.showPopup()

            self.mainImagePoint = (x, y)
            
            # Display zoom window at clicked position and freeze it
            img = self.parent.quadFold.orig_img
            self.drawDoubleZoomImage(x, y, img)
            
            # Enter waiting state - zoom window is now frozen
            self.state = DoubleZoomWidgetState.WAITING_ZOOM_CLICK

        elif mouse_event.inaxes == self.doubleZoomAxes:
            # User clicked zoom window
            self.doubleZoomPoint = (x, y)
            # Enter completed state, precise coordinates available
            self.state = DoubleZoomWidgetState.COMPLETED

    def handle_mouse_scroll_event(self, mouse_event):
        pass

    def is_no_action_state(self, mouse_event):
        """
        Check if other operations should be blocked.
        
        When DoubleZoom is waiting for user to click zoom window, block other operations.
        This method is kept for backward compatibility with SetCentDialog.py.
        
        Args:
            mouse_event: matplotlib mouse event (unused, kept for compatibility)
        
        Returns:
            bool: True if other operations should be blocked
        """
        # Simplified logic: block when waiting for zoom window click
        return self.state == DoubleZoomWidgetState.WAITING_ZOOM_CLICK
    
    def is_blocking_other_actions(self) -> bool:
        """
        New, clearer method name for checking if other operations should be blocked.
        
        Returns:
            bool: True if DoubleZoom is waiting for user action and should block other handlers
        """
        return self.state == DoubleZoomWidgetState.WAITING_ZOOM_CLICK

    def showPopup(self):
        msg = QMessageBox()
        msg.setInformativeText(
            "Please click on zoomed window on the top right")
        dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")
        msg.setStandardButtons(QMessageBox.Ok)
        msg.setWindowTitle("Double Zoom Guide")
        msg.setStyleSheet("QLabel{min-width: 500px;}")
        msg.setCheckBox(dontShowAgainDoubleZoomMessage)
        msg.exec()
        self.dontShowAgainDoubleZoomMessageResult = dontShowAgainDoubleZoomMessage.isChecked()

    def doubleZoomToOrigCoord(self):
        """
        Compute the new x and y for double zoom to orig coord
        """
        dzx, dzy = self.mainImagePoint
        x, y = self.doubleZoomPoint
        newX = dzx - 10 + x / 10
        newY = dzy - 10 + y / 10

        return (newX, newY)
    
    def get_precise_coords(self) -> tuple:
        """
        Get the precise coordinates after zoom window click.
        
        This method should be called after handle_click() returns 'completed'.
        
        Returns:
            (x, y): Precise image coordinates computed from zoom window click
        
        Raises:
            RuntimeError: If called before zoom window click is completed
        """
        if self.state != DoubleZoomWidgetState.COMPLETED:
            raise RuntimeError(
                "get_precise_coords() called before zoom window click completed. "
                "Check that handle_click() returned 'completed' before calling this method."
            )
        
        return self.doubleZoomToOrigCoord()
    
    def reset_for_next_click(self):
        """
        Reset state to prepare for the next click.
        
        This should be called after precise coordinates have been retrieved
        and used, to allow DoubleZoom to handle the next click.
        
        Typical usage:
            status = self.doubleZoom.handle_click(event)
            if status == 'completed':
                x, y = self.doubleZoom.get_precise_coords()
                # Use the coordinates...
                self.doubleZoom.reset_for_next_click()
        """
        # Reset state to idle (ready for next click)
        self.state = DoubleZoomWidgetState.IDLE
        
        # Note: We don't reset mainImagePoint and doubleZoomPoint
        # as they might be useful for debugging or display purposes

    def drawBlueDot(self, x, y, ax):
        self.remove_image_lines(ax=ax, labels=["DoubleZoom Blue Dot"])

        # Plot a blue dot at the given coordinates
        ax.plot(x, y, 'bo', markersize=2, label="DoubleZoom Blue Dot")

    def drawRedDot(self, x, y, ax):
            axis_size = 1

            x_min, x_max = ax.get_xlim()
            y_min, y_max = ax.get_ylim()

            # Clamp values so they stay inside axis bounds
            x1 = max(x_min, min(x - axis_size, x_max))
            x2 = max(x_min, min(x + axis_size, x_max))
            y1 = max(y_min, min(y - axis_size, y_max))
            y2 = max(y_min, min(y + axis_size, y_max))

            self.remove_image_lines(ax=ax, labels=["DoubleZoom Red Dot"])

            ax.plot((x1, x2), (y1, y2), color='r', label="DoubleZoom Red Dot")
            ax.plot((x1, x2), (y2, y1), color='r', label="DoubleZoom Red Dot")

    def drawDoubleZoomImage(self, x, y, img):
        """
        Update zoom window content at position (x, y).
        Uses set_data() to update existing image instead of creating new ones.
        """
        if x > 10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
            ax = self.doubleZoomAxes
            imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                
                # If image object doesn't exist, create it; otherwise update it
                if self.doubleZoomImage is None:
                    self.doubleZoomImage = self.doubleZoomAxes.imshow(imgScaled)
                    self.doubleZoomAxes.invert_yaxis()
                else:
                    # Update existing image data instead of creating new one
                    self.doubleZoomImage.set_data(imgScaled)

                # Clean up red dot markers
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() == "Red Dot":
                            ax.lines[i].remove()

def main():
    app = QApplication(sys.argv)
    widget = DoubleZoomWidget(None, None)
    widget.show()
    app.exec()


if __name__ == '__main__':
    main()
