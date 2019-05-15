# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'launcher.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from .pyqt_utils import *

class Ui_LauncherForm(object):
    def setupUi(self, LauncherForm):
        LauncherForm.setObjectName("LauncherForm")
        LauncherForm.resize(800, 600)
        self.listWidget = QListWidget(LauncherForm)
        self.listWidget.setGeometry(QRect(10, 10, 251, 541))
        self.listWidget.setAutoFillBackground(True)
        self.listWidget.setTabKeyNavigation(True)
        self.listWidget.setObjectName("listWidget")
        item = QListWidgetItem()
        brush = QBrush(QColor(0, 0, 0))
        brush.setStyle(Qt.NoBrush)
        item.setBackground(brush)
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        item = QListWidgetItem()
        item.setFlags(Qt.ItemIsSelectable|Qt.ItemIsUserCheckable|Qt.ItemIsEnabled)
        self.listWidget.addItem(item)
        self.stackedWidget = QStackedWidget(LauncherForm)
        self.stackedWidget.setGeometry(QRect(270, 10, 521, 541))
        self.stackedWidget.setObjectName("stackedWidget")
        self.page = QWidget()
        self.page.setObjectName("page")
        self.textBrowser = QTextBrowser(self.page)
        self.textBrowser.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser.setOpenExternalLinks(True)
        self.textBrowser.setObjectName("textBrowser")
        self.stackedWidget.addWidget(self.page)
        self.page_2 = QWidget()
        self.page_2.setObjectName("page_2")
        self.textBrowser_2 = QTextBrowser(self.page_2)
        self.textBrowser_2.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_2.setOpenExternalLinks(True)
        self.textBrowser_2.setObjectName("textBrowser_2")
        self.stackedWidget.addWidget(self.page_2)
        self.page_3 = QWidget()
        self.page_3.setObjectName("page_3")
        self.textBrowser_3 = QTextBrowser(self.page_3)
        self.textBrowser_3.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_3.setOpenExternalLinks(True)
        self.textBrowser_3.setObjectName("textBrowser_3")
        self.stackedWidget.addWidget(self.page_3)
        self.page_4 = QWidget()
        self.page_4.setObjectName("page_4")
        self.textBrowser_4 = QTextBrowser(self.page_4)
        self.textBrowser_4.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_4.setOpenExternalLinks(True)
        self.textBrowser_4.setObjectName("textBrowser_4")
        self.stackedWidget.addWidget(self.page_4)
        self.page_5 = QWidget()
        self.page_5.setObjectName("page_5")
        self.textBrowser_5 = QTextBrowser(self.page_5)
        self.textBrowser_5.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_5.setOpenExternalLinks(True)
        self.textBrowser_5.setObjectName("textBrowser_5")
        self.stackedWidget.addWidget(self.page_5)
        self.page_6 = QWidget()
        self.page_6.setObjectName("page_6")
        self.textBrowser_6 = QTextBrowser(self.page_6)
        self.textBrowser_6.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_6.setOpenExternalLinks(True)
        self.textBrowser_6.setObjectName("textBrowser_6")
        self.stackedWidget.addWidget(self.page_6)
        self.page_7 = QWidget()
        self.page_7.setObjectName("page_7")
        self.textBrowser_7 = QTextBrowser(self.page_7)
        self.textBrowser_7.setGeometry(QRect(0, 0, 521, 541))
        self.textBrowser_7.setOpenExternalLinks(True)
        self.textBrowser_7.setObjectName("textBrowser_7")
        self.stackedWidget.addWidget(self.page_7)
        self.layoutWidget = QWidget(LauncherForm)
        self.layoutWidget.setGeometry(QRect(10, 560, 781, 31))
        self.layoutWidget.setObjectName("layoutWidget")
        self.horizontalLayout = QHBoxLayout(self.layoutWidget)
        self.horizontalLayout.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.quitButton = QPushButton(self.layoutWidget)
        self.quitButton.setObjectName("quitButton")
        self.horizontalLayout.addWidget(self.quitButton)
        spacerItem = QSpacerItem(40, 20, QSizePolicy.Expanding, QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.runButton = QPushButton(self.layoutWidget)
        self.runButton.setObjectName("runButton")
        self.testButton = QPushButton(self.layoutWidget)
        self.testButton.setObjectName("testButton")
        self.horizontalLayout.addWidget(self.runButton)
        self.horizontalLayout.addWidget(self.testButton)

        self.retranslateUi(LauncherForm)
        self.listWidget.setCurrentRow(0)
        self.stackedWidget.setCurrentIndex(0)
        self.listWidget.currentRowChanged['int'].connect(self.stackedWidget.setCurrentIndex)
        self.quitButton.clicked.connect(LauncherForm.close)
        QMetaObject.connectSlotsByName(LauncherForm)

    def retranslateUi(self, LauncherForm):
        _translate = QCoreApplication.translate
        LauncherForm.setWindowTitle(_translate("LauncherForm", "MuscleX Launcher"))
        __sortingEnabled = self.listWidget.isSortingEnabled()
        self.listWidget.setSortingEnabled(False)
        item = self.listWidget.item(0)
        item.setText(_translate("LauncherForm", "Equator"))
        item = self.listWidget.item(1)
        item.setText(_translate("LauncherForm", "Quadrant Folding"))
        item = self.listWidget.item(2)
        item.setText(_translate("LauncherForm", "Projection Traces"))
        item = self.listWidget.item(3)
        item.setText(_translate("LauncherForm", "Scanning Diffraction"))
        item = self.listWidget.item(4)
        item.setText(_translate("LauncherForm", "Image Merger"))
        item = self.listWidget.item(5)
        item.setText(_translate("LauncherForm", "Diffraction Centroids"))
        item = self.listWidget.item(6)
        item.setText(_translate("LauncherForm", "DDF Processor"))
        self.listWidget.setSortingEnabled(__sortingEnabled)
        self.textBrowser.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">The purpose of the </span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Equator</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> program is to analyze the equatorial portion of muscle X-ray diffraction patterns.</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/Equator/Equator-(eq).html\"><span style=\" font-size:10pt; text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/Equator/Equator-(eq).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_2.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">The equator and the meridian of a fiber diffraction pattern divides a pattern into four quadrants. You can regenerate a full diffraction pattern by simply rotating the summed quadrant. </span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Quadrant Folding</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> is a program for generating such a quadrant-folded image.</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/QuadrantFolding/Quadrant-Folding-(qf).html\"><span style=\" font-size:10pt; text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/QuadrantFolding/Quadrant-Folding-(qf).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_3.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Projection traces</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> was conceived originally as a program to extract the integrated intensity along a layer line in order to identify positions of intensity maxima along with their integrated intensities. It also allows measurement of the radial width of meridional reflections.</span></p>\n"
"<p align=\"justify\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p align=\"justify\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/ProjectionTraces/Projection-Traces-(pt).html\"><span style=\" font-size:10pt; text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/ProjectionTraces/Projection-Traces-(pt).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_4.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Scanning diffraction</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> imaging experiments attempt to determine the distribution of diffracting materials such as collagen, myelin, and amyloid structures as a function of position in a sample that is raster scanned in a microbeam.</span></p>\n"
"<p align=\"justify\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p align=\"justify\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/ScanningDiffraction/Scanning-Diffraction-(di).html\"><span style=\" text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/ScanningDiffraction/Scanning-Diffraction-(di).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_5.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Image Merger </span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">is a program which is designed to be used with a series of images with sequential file names taken in a time resolved experiment.</span></p>\n"
"<p align=\"justify\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p align=\"justify\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/ImageMerger/Image-Merger-(im).html\"><span style=\" text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/ImageMerger/Image-Merger-(im).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_6.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">Diffraction Centroids</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> is designed to rapidly and accurately measure the spacings of user specified meridional reflections as well as the 5.9 and 5.1 actin layer lines in a series of diffraction images, such as those generated in a time resolved experiment.</span></p>\n"
"<p align=\"justify\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p align=\"justify\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/DiffractionCentroids/Diffraction-Centroids-(dc).html\"><span style=\" text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/DiffractionCentroids/Diffraction-Centroids-(dc).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.textBrowser_7.setHtml(_translate("LauncherForm", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; font-weight:600; color:#24292e; background-color:#ffffff;\">DDF Processor</span><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> is a program which is able to average data points for ddf file.</span></p>\n"
"<p align=\"justify\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e;\"><br /></p>\n"
"<p align=\"justify\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\">See </span><a href=\"https://musclex.readthedocs.io/en/latest/AppSuite/DDFProcessor/DDF-Processor-(ddf).html\"><span style=\" text-decoration: underline; color:#0000ff;\">https://musclex.readthedocs.io/en/latest/AppSuite/DDFProcessor/DDF-Processor-(ddf).html</span></a><span style=\" font-family:\'-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol\'; font-size:10pt; color:#24292e; background-color:#ffffff;\"> for details.</span></p></body></html>"))
        self.quitButton.setText(_translate("LauncherForm", "Quit"))
        self.runButton.setText(_translate("LauncherForm", "Run"))
        self.testButton.setText(_translate("LauncherForm", "Run Tests"))
