import sys
import math
import os
from PyQt5 import QtWidgets, QtGui, QtCore

class ImageWidget(QtWidgets.QWidget):
    # Signal to communicate updated center coordinates (as text)
    centerChanged = QtCore.pyqtSignal(str)

    def __init__(self, image_path, parent=None):
        super().__init__(parent)
        # Set a fixed widget size (canvas size)
        self.setFixedSize(800, 600)
        
        # Load the image as a QImage
        print(f"Loading image from: {image_path}")
        print("Exists?", os.path.exists(image_path))
        self.image = QtGui.QImage(image_path)
        if self.image.isNull():
            raise Exception(f"Unable to load image from: {image_path}")
        self.image_width = self.image.width()
        self.image_height = self.image.height()
        
        # Start with the original image center mapped to the canvas center
        self.current_center = QtCore.QPointF(self.image_width / 2, self.image_height / 2)
        self.angle = 0.0  # in radians
        self.scale = 1.0  # will be computed
        self.update_scale()

    def update_scale(self):
        """
        Compute the scale factor so that after applying rotation, the entire image
        is visible within the widget. This is done by checking the positions
        of all four corners relative to the current center.
        """
        cos_a = math.cos(self.angle)
        sin_a = math.sin(self.angle)
        corners = [
            QtCore.QPointF(0, 0),
            QtCore.QPointF(self.image_width, 0),
            QtCore.QPointF(self.image_width, self.image_height),
            QtCore.QPointF(0, self.image_height)
        ]
        max_rx = 0
        max_ry = 0
        for corner in corners:
            dx = corner.x() - self.current_center.x()
            dy = corner.y() - self.current_center.y()
            # Rotate the offset by the current angle.
            rx = cos_a * dx - sin_a * dy
            ry = sin_a * dx + cos_a * dy
            max_rx = max(max_rx, abs(rx))
            max_ry = max(max_ry, abs(ry))
        # Compute the maximum uniform scale to fit the rotated image inside the widget.
        if max_rx == 0 or max_ry == 0:
            new_scale = 1.0
        else:
            new_scale = min((self.width() / 2) / max_rx, (self.height() / 2) / max_ry)
        self.scale = new_scale



    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        # Build and set the transformation to draw the image.
        transform = QtGui.QTransform()
        canvas_center_x = self.width() / 2
        canvas_center_y = self.height() / 2
        transform.translate(canvas_center_x, canvas_center_y)
        transform.rotate(math.degrees(self.angle))
        transform.scale(self.scale, self.scale)
        transform.translate(-self.current_center.x(), -self.current_center.y())
        painter.setTransform(transform)
        painter.drawImage(0, 0, self.image)
    
        # Reset the transformation so that the crosshair is drawn in widget coordinates.
        painter.resetTransform()
    
        # Set up the pen for drawing the crosshair (red, 2 pixels wide).
        pen = QtGui.QPen(QtCore.Qt.red, 2)
        painter.setPen(pen)
    
        # Compute the center point of the widget.
        center_x = self.width() / 2
        center_y = self.height() / 2
    
        # Draw a small crosshair (e.g., 20 pixels in length).
        painter.drawLine(int(center_x - 10), int(center_y), int(center_x + 10), int(center_y))
        painter.drawLine(int(center_x), int(center_y - 10), int(center_x), int(center_y + 10))
    
        painter.end()



    def paintEventALT(self, event):
        """
        Render the image using a QPainter with a transformation.
        The transformation is constructed so that:
          - The point 'current_center' in the original image maps to the center of the widget.
          - The image is rotated by self.angle (in degrees) and uniformly scaled by self.scale.
        """
        painter = QtGui.QPainter(self)
        # Build the transformation: 
        # T = translate(canvas_center) * rotate(angle) * scale(scale, scale) * translate(-current_center)
        transform = QtGui.QTransform()
        canvas_center_x = self.width() / 2
        canvas_center_y = self.height() / 2
        transform.translate(canvas_center_x, canvas_center_y)
        transform.rotate(math.degrees(self.angle))
        transform.scale(self.scale, self.scale)
        transform.translate(-self.current_center.x(), -self.current_center.y())
        painter.setTransform(transform)
        painter.drawImage(0, 0, self.image)
        painter.end()

    def mousePressEvent(self, event):
        """
        Map the mouse click (in widget coordinates) back to the original image coordinates
        using the inverse transform:
        
          original = current_center + (1/scale) * R(-angle) * (clicked_point - canvas_center)
          
        Update the current center accordingly, recompute the scale and update the display.
        """
        canvas_center = QtCore.QPointF(self.width() / 2, self.height() / 2)
        click = QtCore.QPointF(event.x(), event.y())
        dx = click.x() - canvas_center.x()
        dy = click.y() - canvas_center.y()
        # For the inverse rotation, note that cos(-angle)=cos(angle) and sin(-angle)=-sin(angle)
        cos_a = math.cos(self.angle)
        sin_a = math.sin(self.angle)
        orig_dx = (dx * cos_a + dy * sin_a) / self.scale
        orig_dy = (-dx * sin_a + dy * cos_a) / self.scale
        new_center = QtCore.QPointF(self.current_center.x() + orig_dx,
                                    self.current_center.y() + orig_dy)
        self.current_center = new_center
        self.update_scale()
        # Emit the updated center (in original image coordinates) for display.
        self.centerChanged.emit(f"Center: ({self.current_center.x():.1f}, {self.current_center.y():.1f})")
        self.update()

    def rotateImage(self):
        """
        Rotate the image 45Â° (Ï€/4 radians) about its current center.
        Recompute the scale so that the rotated image is fully visible.
        """
        self.angle += math.radians(45)
        self.update_scale()
        self.centerChanged.emit(f"Center: ({self.current_center.x():.1f}, {self.current_center.y():.1f})")
        self.update()


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, image_path):
        super().__init__()
        self.setWindowTitle("PyQt5 Image Viewer â€“ Click to Recenter, Button to Rotate")
        
        # Set up the central widget and layout.
        central_widget = QtWidgets.QWidget()
        self.setCentralWidget(central_widget)
        layout = QtWidgets.QVBoxLayout(central_widget)
        
        # Create the image widget.
        self.image_widget = ImageWidget(image_path)
        layout.addWidget(self.image_widget)
        
        # Create a control layout with a rotate button and a label to show the center.
        control_layout = QtWidgets.QHBoxLayout()
        self.rotate_button = QtWidgets.QPushButton("Rotate 45Â°")
        self.rotate_button.clicked.connect(self.image_widget.rotateImage)
        control_layout.addWidget(self.rotate_button)
        self.center_label = QtWidgets.QLabel(
            f"Center: ({self.image_widget.current_center.x():.1f}, {self.image_widget.current_center.y():.1f})")
        control_layout.addWidget(self.center_label)
        layout.addLayout(control_layout)
        
        # Connect the image widget signal to update the label.
        self.image_widget.centerChanged.connect(self.center_label.setText)


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    # Ensure you have an image file named "example.tif" in the same folder.
    window = MainWindow("Python.jpg")
    window.show()
    sys.exit(app.exec_())