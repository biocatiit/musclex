import sys
import os
import numpy as np
import glob
import fabio
from functools import partial
from PyQt5.QtWidgets import (QGroupBox, QDialog, QWidget,
QScrollArea, QGridLayout, QLabel, QCheckBox, QVBoxLayout,
QDoubleSpinBox, QPushButton, QHBoxLayout, QSizePolicy, QMessageBox)
from PyQt5.QtGui import QPixmap, QImage
from PyQt5.QtCore import Qt
from .XRayViewerGUI import XRayViewerGUI


class AISEImageSelectionWindow(QDialog):

    def __init__(self, dir_path, orig_img_list, img_grps, misaligned_images, isHDF5):
        super().__init__(None)
        self.setWindowTitle("Image Sequence Selection")
        self.isHDF5 = isHDF5
        self.img_list = orig_img_list
        self.dir_path = dir_path
        self.thumbnail_labels = []
        self.checkbox_list = []
        self.misaligned_images = misaligned_images
        self.qlabel_list = []
        self.img_grps = img_grps
        self.img_to_delete = []
        self.selectingImage = False
        self.firstImage = None
        self.showInstructions = True
        self.editingMode = False
        self.XRayViewer = None
        self.selectionMode = False
        print(self.dir_path)

        self.initUI()
        self.resize(1200,750)
        
        self.setConnections()


    def initUI(self):
        self.scroll = QScrollArea()
        
        self.createListWidget()

        self.scroll.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.scroll.setWidgetResizable(True)
        #self.scroll.setVisible(False)

        self.scroll.setWidget(self.list_widget)
        #self.scroll.setWidget(self.thumbnail_widget)

        self.layout = QHBoxLayout(self)
        self.layout.addWidget(self.scroll)

        # Display Options

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.dispOptLayout = QGridLayout()

        self.spminInt = QDoubleSpinBox()
        self.spminInt.setToolTip("Reduction in the maximal intensity shown \
            to allow for more details in the image.")
        self.spminInt.setKeyboardTracking(False)
        self.spminInt.setSingleStep(5)
        self.spminInt.setDecimals(0)
        self.spmaxInt = QDoubleSpinBox()
        self.spmaxInt.setToolTip("Increase in the minimal intensity shown \
            to allow for more details in the image.")
        self.spmaxInt.setKeyboardTracking(False)
        self.spmaxInt.setSingleStep(5)
        self.spmaxInt.setDecimals(0)

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.toggleThumbnailChkBx = QCheckBox("Toggle Thumbnails (SLOW)")
        self.toggleThumbnailChkBx.setChecked(False)

        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.toggleThumbnailChkBx, 3, 0, 1, 2)
        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.legendGrpBox = QGroupBox()
        self.legendGrpBox.setTitle("Legend")
        self.legendGrpBox.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.legendGrpBoxLayout = QVBoxLayout()

        # Select sequence button
        self.selectSequenceButton = QPushButton("Select Sequence")
        self.selectSequenceButton.setToolTip("Select Sequence")
        
        # Edit sequence button
        self.editSequenceButton = QPushButton("Edit Sequence")
        self.editSequenceButton.setToolTip("Edit Sequence")

        self.resetSequenceButton = QPushButton("Reset Sequence")
        self.resetSequenceButton.setToolTip("Reset Sequence")
        # Confirm Button
        self.confirmButton = QPushButton("Confirm")
        self.confirmButton.setToolTip("Confirm Selection")

        if not self.img_grps:
            self.resetSequenceButton.setEnabled(False)
            self.editSequenceButton.setEnabled(False)
            self.confirmButton.setEnabled(False)

        # Options Layout
        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addWidget(self.selectSequenceButton)
        self.optionsLayout.addWidget(self.resetSequenceButton)
        self.optionsLayout.addWidget(self.editSequenceButton)
        self.optionsLayout.addWidget(self.confirmButton)
        self.optionsLayout.addStretch()

        self.layout.addLayout(self.optionsLayout)

        self.setGeometry(300, 300, 350, 300)
        self.setWindowTitle('Image Sequence Selection')

        #self.load_images(self.dir_path)
        self.load_images(self.dir_path)
        self.show()

    def createThumbnailWidget(self):
        self.thumbnail_widget = QWidget()
        self.grid_layout = QGridLayout()
        self.thumbnail_widget.setLayout(self.grid_layout)
    
    def createListWidget(self):
        self.list_widget = QWidget()
        self.vertical_layout = QVBoxLayout()
        self.list_widget.setLayout(self.vertical_layout)

    def setConnections(self):
        self.spminInt.valueChanged.connect(self.update_thumbnail_intensities)
        self.spmaxInt.valueChanged.connect(self.update_thumbnail_intensities)
        self.toggleThumbnailChkBx.clicked.connect(self.toggleThumbnailsClicked)    
        self.selectSequenceButton.clicked.connect(self.onSelectImageClicked)
        self.confirmButton.clicked.connect(self.confirmedClicked)
        self.resetSequenceButton.clicked.connect(self.resetSequenceClicked)
        self.editSequenceButton.clicked.connect(self.editSequenceClicked)

    

    def confirmedClicked(self):
        if self.XRayViewer is not None:
            self.XRayViewer.close()
        self.accept()

    def editSequenceClicked(self):
        if self.editingMode is False:
            self.editingMode = True
            self.load_checkboxes(True)
            self.editSequenceButton.setText("Finish Editing")
            self.editSequenceButton.setStyleSheet("color: #ededed; background-color: #af6207")
        elif self.editingMode is True:
            self.editingMode = False
            self.editSequenceButton.setText("Edit Sequence")
            self.editSequenceButton.setStyleSheet("color: black; background-color: light gray")
            if self.img_to_delete:
                self.delete_images_from_grp()
            self.clear_checkboxes()

    def resetSequenceClicked(self):
        self.img_grps = []
        self.editSequenceButton.setText("Edit Sequence")
        self.editSequenceButton.setStyleSheet("color: black; background-color: light gray")
        self.editingMode = False
        self.img_to_delete = []
        self.editSequenceButton.setEnabled(False)
        self.confirmButton.setEnabled(False)
        self.resetSequenceButton.setEnabled(False)
        for checkbox in self.checkbox_list:
            checkbox.setChecked(False)
        self.clear_checkboxes()

    # TODO CHANGE IT INTO A LIST FORMAT
    def toggleThumbnailsClicked(self):
        if self.toggleThumbnailChkBx.isChecked():
            self.createThumbnailWidget()
            self.scroll.setWidget(self.thumbnail_widget)
        else:
            self.createListWidget()
            self.scroll.setWidget(self.list_widget)
        self.load_images(self.dir_path)

    def open_message_box(self):
        msg_box = QMessageBox()
        msg_box.setIcon(QMessageBox.Information)
        msg_box.setText("Select the FIRST and LAST image of a sequence.")
        msg_box.setWindowTitle("Warning")
        msg_box.setStandardButtons(QMessageBox.Ok)
        cb = QCheckBox('Do not show this message again')
        cb.stateChanged.connect(self.showAgain)
        msg_box.setCheckBox(cb)
        retval = msg_box.exec_()

    def delete_images_from_grp(self):
        for img in self.img_to_delete:
            for grp in self.img_grps:
                if img in grp:
                    grp.remove(img)
        self.img_to_delete = []

    def showAgain(self):
        self.showInstructions = False

    def onSelectImageClicked(self):
        self.selectingImage = True
        if self.showInstructions is True:
            self.open_message_box()
        self.load_checkboxes(False)

    def checkboxChecked(self, index):
        if self.editingMode is False:
            new_grp = []
            if self.firstImage is None:
                if index == len(self.img_list)-1:
                    new_grp.append(self.img_list[index])
                else:
                    self.firstImage = index
                    return
            else: # Second image selected, check if it is the same length as previous groups.
                for i in range(self.firstImage, index+1):
                    self.checkbox_list[i].setChecked(True)
                    new_grp.append(self.img_list[i])
            self.img_grps.append(new_grp)
            self.firstImage = None
            self.clear_checkboxes()
            self.confirmButton.setEnabled(True)
            self.resetSequenceButton.setEnabled(True)
            self.editSequenceButton.setEnabled(True)
        else:
            if self.checkbox_list[index].isChecked():
                self.img_to_delete.remove(self.img_list[index])
            elif self.checkbox_list[index].isChecked() is False:
                self.img_to_delete.append(self.img_list[index])

    def clear_checkboxes(self):
        for checkbox in self.checkbox_list:
            checkbox.setEnabled(False)

    
    def load_images(self, folder_path):
        self.thumbnail_labels.clear()
        print(self.misaligned_images)

        # List all TIFF files in the folder
        if self.isHDF5:
            try:
                with fabio.open(folder_path) as series:
                    for frame in series.frames():
                        label = QLabel(self)
                        widget = QWidget()
                        
                        checkbox = QCheckBox("Select")
                        checkbox.setEnabled(False)
                        checkbox.clicked.connect(partial(self.checkboxChecked, frame.index))
                        
                        namefile = os.path.split(frame.file_container.filename)[1].split('.')
                        temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                        short_filename = (temp_filename[:10] + '...') if len(temp_filename) > 10 else temp_filename
                        file_label = QLabel(short_filename)
                        file_label.setToolTip(temp_filename)
                        
                        if any(self.img_list[frame.index] in sublist for sublist in self.img_grps):
                            checkbox.setChecked(True)
                            
                        if temp_filename in self.misaligned_images:
                            widget.setStyleSheet("border: 4px solid red")
                            
                        self.checkbox_list.append(checkbox)
                        
                        if (self.toggleThumbnailChkBx.isChecked()):
                            image = frame.data.astype(np.float32)
                            min_val = image.min()
                            max_val = image.max()
                            self.spmaxInt.setRange(min_val, max_val)
                            self.spminInt.setRange(min_val, max_val)
                            self.spmaxInt.setValue(max_val * .5)
                            self.spminInt.setValue(min_val)

                            self.minIntLabel.setText("Min Intensity ("+str(min_val)+")")
                            self.maxIntLabel.setText("Max Intensity (" + str(max_val) + ")")

                            vbox = QVBoxLayout()
                            image = self.normalizeImage(image, None, None)
                            q_image = QImage(image.data, image.shape[1], image.shape[0], QImage.Format_Indexed8)
                            pixmap = QPixmap.fromImage(q_image)            
                            label.setPixmap(pixmap.scaled(100, 100, Qt.KeepAspectRatio))
                            self.thumbnail_labels.append(label)  # Add label to the list
                            vbox.addWidget(label)
                            vbox.addWidget(file_label)
                            vbox.addWidget(checkbox)
                            vbox.addStretch()
                            self.qlabel_list.append(label)
                            label.setProperty("fileName", temp_filename)
                            label.mousePressEvent = lambda event, i=frame.index, label=label: self.onLabelClicked(event, i, label)
                            widget.setLayout(vbox)
                            row = frame.index // 3  # Change '3' to the desired number of columns
                            column = frame.index % 3  # Change '3' to the desired number of columns
                            self.grid_layout.addWidget(widget, row, column)
                        else:
                            hbox = QHBoxLayout()
                            file_label.setProperty("fileName", temp_filename)
                            file_label.mousePressEvent = lambda event, i=frame.index, label=file_label: self.onLabelClicked(event, i, label)
                            
                            #TODO figure out why the text is still being truncated
                            file_label.setFixedHeight(20)
                            checkbox.setFixedHeight(20)
                            file_label.setAlignment(Qt.AlignLeft)
                            file_label.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
                            
                            hbox.addWidget(file_label)
                            hbox.addWidget(checkbox)
                            widget.setLayout(hbox)
                            widget.setFixedHeight(30)
                            
                            self.vertical_layout.setSpacing(0)
                            self.vertical_layout.setContentsMargins(0, 0, 0, 0)
                            self.vertical_layout.addWidget(widget)
            except Exception as e:
                infMsg = QMessageBox()
                infMsg.setText('Error opening file: ' + folder_path)
                infMsg.setInformativeText("File is not a valid HDF5 file or corrupted.")
                infMsg.setStandardButtons(QMessageBox.Ok)
                infMsg.setIcon(QMessageBox.Information)
                print(e)
                infMsg.exec_()
        else:
            tiff_files = glob.glob(os.path.join(folder_path, '*.tif'))
            for i, tiff_file in enumerate(tiff_files):
                label = QLabel(self)
                widget = QWidget()
                
                # File name and checkbox
                filename = os.path.basename(tiff_file)
                short_filename = (filename[:10] + '...') if len(filename) > 10 else filename
                file_label = QLabel(short_filename)
                file_label.setToolTip(filename)  # Set tooltip to display full filename on hover

                checkbox = QCheckBox("Select")
                checkbox.setEnabled(False)
                checkbox.clicked.connect(partial(self.checkboxChecked, i))

                if any(self.img_list[i] in sublist for sublist in self.img_grps):
                    checkbox.setChecked(True)

                if tiff_file in self.misaligned_images:
                    widget.setStyleSheet("border: 4px solid red")

                self.checkbox_list.append(checkbox)

                if (self.toggleThumbnailChkBx.isChecked()):
                    image = fabio.open(tiff_file).data.astype(np.float32)
                    min_val = image.min()
                    max_val = image.max()
                    self.spmaxInt.setRange(min_val, max_val)
                    self.spminInt.setRange(min_val, max_val)
                    self.spmaxInt.setValue(max_val * .5)
                    self.spminInt.setValue(min_val)

                    self.minIntLabel.setText("Min Intensity ("+str(min_val)+")")
                    self.maxIntLabel.setText("Max Intensity (" + str(max_val) + ")")

                    vbox = QVBoxLayout()
                    image = self.normalizeImage(image, None, None)
                    q_image = QImage(image.data, image.shape[1], image.shape[0], QImage.Format_Indexed8)
                    pixmap = QPixmap.fromImage(q_image)            
                    label.setPixmap(pixmap.scaled(100, 100, Qt.KeepAspectRatio))
                    self.thumbnail_labels.append(label)  # Add label to the list
                    vbox.addWidget(label)
                    vbox.addWidget(file_label)
                    vbox.addWidget(checkbox)
                    vbox.addStretch()
                    self.qlabel_list.append(label)
                    label.setProperty("fileName", tiff_file)
                    label.mousePressEvent = lambda event, i=i, label=label: self.onLabelClicked(event, i, label)
                    widget.setLayout(vbox)
                    row = i // 3  # Change '3' to the desired number of columns
                    column = i % 3  # Change '3' to the desired number of columns
                    self.grid_layout.addWidget(widget, row, column)
                else:
                    hbox = QHBoxLayout()
                    file_label.setProperty("fileName", tiff_file)
                    file_label.mousePressEvent = lambda event, i=i, label=file_label: self.onLabelClicked(event, i, label)
                    
                    #TODO figure out why the text is still being truncated
                    file_label.setFixedHeight(20)
                    checkbox.setFixedHeight(20)
                    file_label.setAlignment(Qt.AlignLeft)
                    file_label.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
                    
                    hbox.addWidget(file_label)
                    hbox.addWidget(checkbox)
                    widget.setLayout(hbox)
                    widget.setFixedHeight(30)
                    
                    self.vertical_layout.setSpacing(0)
                    self.vertical_layout.setContentsMargins(0, 0, 0, 0)
                    self.vertical_layout.addWidget(widget)


    # Handles the click event on the thumbnail label.
    # Opens the XRayViewerGUI and loads the selected image
    # Loads a new image if XRayViewerGUI is already open

    def onLabelClicked(self, event, index, label):
        if event.button() == Qt.LeftButton:
            print(self.img_list[index])
            print(label.property("fileName"))
            print("opening xv")
            if self.XRayViewer is not None:
                self.XRayViewer.close()
            self.XRayViewer = XRayViewerGUI()
            self.XRayViewer.show()
            if self.isHDF5:
                self.XRayViewer.onNewFileSelected(self.dir_path)
                self.XRayViewer.currentFileNumber = index
                self.XRayViewer.onImageChanged()
            else:
                self.XRayViewer.onNewFileSelected(label.property("fileName"))
    

    def load_checkboxes(self, editing):
        for i, checkbox in enumerate(self.checkbox_list):
            if editing is False:
                if not any(self.img_list[i] in sublist for sublist in self.img_grps):
                    checkbox.setEnabled(True)
            else:
                if any(self.img_list[i] in sublist for sublist in self.img_grps):
                    checkbox.setEnabled(True)


    '''
    Normalizes the 32 bit image to an 8 bit image so QImage can read it
    Uses the default max/min values if vmax and vmin are None
    Otherwise, rescales it with vmax and vmin
    '''
    def normalizeImage(self, image, vmax, vmin):
        if vmax is None or vmin is None:
            image = image - np.min(image)
            image = image/ np.max(image)
        else:
            image = image - vmin
            image = image / vmax
        image = image * 255
        image = image.astype(np.uint8)
        return image
    
    # TODO find a way to convert qpixmap -> qimage -> numpy array to apply the normalization without losing quality
    def update_thumbnail_intensities(self):
        tiff_files = glob.glob(os.path.join(self.dir_path, '*.tif'))
        for i, label in enumerate(self.thumbnail_labels):
            image = fabio.open(tiff_files[i]).data.astype(np.float32)
            vmin = self.spminInt.value()
            vmax = self.spmaxInt.value()
            image = self.normalizeImage(image, vmax, vmin)
            # Convert to qImage
            q_image = QImage(image.data, image.shape[1], image.shape[0], QImage.Format_Indexed8)
            pixmap = QPixmap.fromImage(q_image)
            label.setPixmap(pixmap.scaled(100, 100, Qt.KeepAspectRatio))

    #def setConnections(self):
