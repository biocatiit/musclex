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

import os
from os.path import split, exists, join
import numpy as np
import fabio
#from ..ui.pyqt_utils import *
from .hdf5_manager import loadFile
from PyQt5.QtWidgets import QMessageBox

input_types = ['adsc', 'cbf', 'edf', 'fit2d', 'mar345', 'marccd', 'hdf5', 'h5', 'pilatus', 'tif', 'tiff', 'smv']

def getFilesAndHdf(dir_path):
    """
    Give the image files and hdf files in a folder selected
    :param dir_path: directory path
    :return: image list, hdf list
    """
    fileList = os.listdir(dir_path)
    imgList = []
    hdfList = []

    for f in fileList:
        full_file_name = fullPath(dir_path, f)
        if isImg(full_file_name):
            imgList.append(f)
        else:
            toks = f.split('.')
            if toks[-1] == 'hdf':
                hdfList.append(f)

    return imgList, hdfList

def getBlankImageAndMask(path):
    """
    Give the blank image and the mask threshold saved in settings
    :return: blankImage, mask threshold
    """
    mask_file = join(join(path, 'settings'),'mask.tif')
    blank_file = join(join(path, 'settings'),'blank.tif')
    mask = None
    blank_img = None
    if exists(mask_file):
        mask = fabio.open(mask_file).data
    if exists(blank_file):
        blank_img = fabio.open(blank_file).data
    return blank_img, mask

def getMaskOnly(path):
    """
    Give only the mask threshold
    :param path: file path
    :return: mask threshold
    """
    maskonly_file = join(join(path, 'settings'),'maskonly.tif')
    if exists(maskonly_file):
        return fabio.open(maskonly_file).data
    return None

def getImgFiles(fullname, headless=False):
    """
    Get directory, all image file names in the same directory and current file index
    :param fullname: full name of the file including directory i.e. /aaa/bbb/ccc/ddd.tif (str)
    :return: directory (str), list of image file names, and current index i.e /aaa/bbb/ccc, ["ddd.tif","eee.tif"], 0
    """
    dir_path, filename = split(str(fullname)) # split directory and file name from full file name
    dir_path = str(dir_path)
    filename = str(filename)
    _, ext = os.path.splitext(str(filename))
    current = 0
    failedcases = []
    filename_index = None

    if ext == ".txt":
        for line in open(fullname, "r"):
            failedcases.append(line.rstrip('\n'))
    else:
        failedcases = None

    if ext in ('.hdf5', '.h5'):
        fileList = loadFile(fullname)
        imgList = []
        if fileList is None or not fileList or None in fileList:
            infMsg = QMessageBox()
            infMsg.setText('Error opening file: ' + fullname)
            infMsg.setInformativeText("File is not a valid HDF5 file or corrupted.")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            return None, None, None, None, None
        for f in fileList[0]:
            if failedcases is not None and f not in failedcases:
                continue
            imgList.append(f)
        if len(imgList) == 1 and not headless:
            # if only one image in the h5 file, take all the single h5 images in the folder
            infMsg = QMessageBox()
            infMsg.setText('Single Image H5 File')
            infMsg.setInformativeText("The H5 file selected contains only one image. All the H5 files in the current folder containing only one image will be regrouped the same way as a folder containing TIF files.\n")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            list_h5_files = os.listdir(dir_path)
            imgList = []
            fileList = [[],[]]
            for f in list_h5_files:
                _, ext2 = os.path.splitext(str(f))
                full_file_name = fullPath(dir_path, f)
                if ext2 in ('.hdf5', '.h5'):
                    file_loader = loadFile(full_file_name)
                    if len(file_loader[0]) == 1:
                        if failedcases is not None and file_loader[0][0] not in failedcases:
                            continue
                        imgList.append(file_loader[0][0])
                        fileList[0].append(file_loader[0][0])
                        fileList[1].append(file_loader[1][0])
                        if full_file_name == fullname:
                            filename_index = file_loader[0][0]
            imgList.sort()
    else:
        fileList = os.listdir(dir_path)
        imgList = []
        for f in fileList:
            if failedcases is not None and f not in failedcases:
                continue
            full_file_name = fullPath(dir_path, f)
            _, ext2 = os.path.splitext(str(f))
            if isImg(full_file_name) and f != "calibration.tif" and ext2 not in ('.hdf5', '.h5'):  #and validateImage(full_file_name):
                imgList.append(f)
        imgList.sort()

    if failedcases is None and imgList:
        if ext in ('.hdf5', '.h5'):
            if filename_index is None:
                current = 0
            else:
                current = imgList.index(filename_index)
        else:
            current = imgList.index(filename)
    
    return dir_path, imgList, current, fileList, ext

def fullPath(filePath, fileName):
    """
    Combine a path and file name to get full file name
    :param filePath: directory (string)
    :param fileName: file name (string)
    :return: filePath/filename (string)
    """
    # if filePath[-1] == '/':
    #     return filePath+fileName
    # else:
    #     return filePath+"/"+fileName
    return os.path.join(filePath, fileName)

def isImg(fileName):
    """
    Check if a file name is an image file
    :param fileName: (str)
    :return: True or False
    """
    nameList = fileName.split('.')
    return nameList[-1] in input_types

def validateImage(fileName, showDialog=True):
    try:
        test = fabio.open(fileName).data
        return True
    except Exception:
        if showDialog:
            infMsg = QMessageBox()
            infMsg.setText('Error opening file: ' + fileName)
            infMsg.setInformativeText("Fabio could not open .TIFF File. File is either corrupt or invalid.")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
        return False

def isHdf5(fileName):
    """
    Check if a file name is an hdf5 file
    :param fileName: (str)
    :return: True or False
    """
    nameList = fileName.split('.')
    return nameList[-1] in ('hdf5', 'h5')

def ifHdfReadConvertless(fileName, img):
    """
    Check if a file name is an hdf5 file
    and convert it to be directly readable without converting to tiff
    :param fileName, img: (str), (array)
    :return: img converted
    """
    if isHdf5(fileName):
        img = img.astype(np.int32)
        img[img==4294967295] = -1
    return img

def createFolder(path):
    """
    Create a folder if it doesn't exist
    :param path: full path of creating directory
    :return:
    """
    if not exists(path):
        os.makedirs(path)
