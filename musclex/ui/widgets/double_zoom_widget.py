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
import cv2
from PySide6.QtCore import Qt, Slot, Signal
from PySide6.QtWidgets import (QApplication,
                               QWidget,
                               QCheckBox,
                               QMessageBox,
                               QVBoxLayout)

from .ui_widget import UIWidget


class DoubleZoomWidget(UIWidget):
    # Signal emitted when precise coordinates are available from zoom window click
    # Args: (x, y) precise coordinates in image space
    precise_coords_ready = Signal(float, float)
    
    def __init__(self,
        imageAxes=None,
        parent=None,
        dontShowMessage=False):
        super().__init__(imageAxes)

        self.parent = parent

        # Simple flag: True when waiting for zoom window click, False otherwise
        self.waiting_for_zoom_click = False

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
    
    def handle_click(self, mouse_event) -> bool:
        """
        Handle click event. Returns True if event was consumed and should block other handlers.
        
        Workflow:
        1. Click main image -> shows/freezes zoom window, returns True (blocks other handlers)
        2. Click zoom window -> emits precise_coords_ready signal, returns False (allows processing)
        
        Args:
            mouse_event: The matplotlib mouse event
        
        Returns:
            bool: True if event should block other handlers (waiting for zoom click)
                  False if event can propagate (idle or coordinates ready)
        
        Usage in event pipeline:
            if self.doubleZoom.is_enabled():
                if self.doubleZoom.handle_click(event):
                    return  # Block further processing, waiting for zoom window click
                # Otherwise, event may have modified coordinates via signal
        """
        if not self.is_enabled():
            return False
        
        # Process the click
        self.handle_mouse_button_release_event(mouse_event)
        
        # Return True to block if we're waiting for zoom window click
        return self.waiting_for_zoom_click

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
            # If waiting for zoom window click, freeze zoom window
            if self.waiting_for_zoom_click:
                # Only draw blue dot on main image, do not update zoom window
                self.drawBlueDot(x, y, self.imageAxes)
                self.imageCanvas.draw_idle()
                return

            # Normal state: draw cursor and update zoom window content in real-time
            self.drawBlueDot(x, y, self.imageAxes)
            self.drawDoubleZoomImage(x, y, img)
            self.imageCanvas.draw_idle()

        elif mouse_event.inaxes == self.doubleZoomAxes:
            # Draw cursor location in zoom using red cross lines.
            self.drawRedDot(x, y, self.doubleZoomAxes)
            self.imageCanvas.draw_idle()

    def handle_mouse_button_release_event(self, mouse_event):
        """
        Handle mouse button release event.
        - Main image click: freeze zoom window, set waiting flag
        - Zoom window click: calculate precise coords, emit signal, reset flag
        """
        if not self.is_running():
            return

        x = mouse_event.xdata
        y = mouse_event.ydata

        if mouse_event.inaxes == self.imageAxes:
            # User clicked main image - freeze zoom window and wait
            if not self.dontShowAgainDoubleZoomMessageResult:
                self.showPopup()

            self.mainImagePoint = (x, y)
            
            # Display zoom window at clicked position and freeze it
            img = self.parent.quadFold.orig_img
            self.drawDoubleZoomImage(x, y, img)
            
            # Set waiting flag - zoom window is now frozen
            self.waiting_for_zoom_click = True

        elif mouse_event.inaxes == self.doubleZoomAxes:
            # User clicked zoom window - calculate and emit precise coordinates
            self.doubleZoomPoint = (x, y)
            
            # Calculate precise coordinates
            precise_x, precise_y = self._calculate_precise_coords()
            
            # Reset waiting flag BEFORE emitting signal
            self.waiting_for_zoom_click = False
            
            # Emit signal with precise coordinates
            self.precise_coords_ready.emit(precise_x, precise_y)

    def handle_mouse_scroll_event(self, mouse_event):
        pass

    def is_no_action_state(self, mouse_event):
        """
        Check if other operations should be blocked.
        
        When DoubleZoom is waiting for user to click zoom window, block other operations.
        This method is kept for backward compatibility.
        
        Args:
            mouse_event: matplotlib mouse event (unused, kept for compatibility)
        
        Returns:
            bool: True if other operations should be blocked
        """
        return self.waiting_for_zoom_click
    
    def is_blocking_other_actions(self) -> bool:
        """
        Check if other operations should be blocked.
        
        Returns:
            bool: True if DoubleZoom is waiting for user action and should block other handlers
        """
        return self.waiting_for_zoom_click

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

    def _calculate_precise_coords(self):
        """
        Calculate precise coordinates from zoom window click.
        Internal method called automatically when zoom window is clicked.
        """
        dzx, dzy = self.mainImagePoint
        x, y = self.doubleZoomPoint
        newX = dzx - 10 + x / 10
        newY = dzy - 10 + y / 10
        return (newX, newY)
    
    # Deprecated methods - kept for backward compatibility
    def doubleZoomToOrigCoord(self):
        """Deprecated: Use _calculate_precise_coords() or connect to precise_coords_ready signal"""
        return self._calculate_precise_coords()
    
    def get_precise_coords(self) -> tuple:
        """
        Deprecated: Coordinates are now automatically emitted via precise_coords_ready signal.
        This method is kept for backward compatibility only.
        """
        return self._calculate_precise_coords()
    
    def reset_for_next_click(self):
        """
        Deprecated: State is now automatically reset after emitting precise_coords_ready signal.
        This method is kept for backward compatibility but does nothing.
        """
        pass

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
        Inherits intensity normalization from main image to match intensity display.
        """
        if x > 10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
            ax = self.doubleZoomAxes
            imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                
                # Get norm from main image to ensure consistent intensity display
                norm = None
                if self.imageAxes and len(self.imageAxes.images) > 0:
                    main_image = self.imageAxes.images[0]
                    norm = main_image.norm
                
                # If image object doesn't exist, create it; otherwise update it
                if self.doubleZoomImage is None:
                    # Use viridis colormap with same norm as main image
                    self.doubleZoomImage = self.doubleZoomAxes.imshow(
                        imgScaled, 
                        cmap='viridis',
                        norm=norm
                    )
                    self.doubleZoomAxes.invert_yaxis()
                else:
                    # Update existing image data and norm
                    self.doubleZoomImage.set_data(imgScaled)
                    self.doubleZoomImage.set_norm(norm)

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
