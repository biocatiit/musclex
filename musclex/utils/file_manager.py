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
import fabio

input_types = ['adsc', 'cbf', 'edf', 'fit2d', 'mar345', 'marccd', 'pilatus', 'tif', 'hdf5', 'smv']

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

def getImgFiles(fullname):
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

    if ext == ".txt":
        for line in open(fullname, "r"):
            failedcases.append(line.rstrip('\n'))
    else:
        failedcases = None

    fileList = os.listdir(dir_path)
    imgList = []

    for f in fileList:
        if failedcases is not None and f not in failedcases:
            continue
        full_file_name = fullPath(dir_path, f)
        if isImg(full_file_name) and f != "calibration.tif":
            imgList.append(f)

    imgList.sort()

    if failedcases is None:
        current = imgList.index(filename)

    return dir_path, imgList, current

def fullPath(filePath, fileName):
    """
    Combine a path and file name to get full file name
    :param filePath: directory (string)
    :param fileName: file name (string)
    :return: filePath/filename (string)
    """
    if filePath[-1] == '/':
        return filePath+fileName
    else:
        return filePath+"/"+fileName

def isImg(fileName):
    """
    Check if a file name is an image file
    :param fileName: (str)
    :return: True or False
    """
    nameList = fileName.split('.')
    return nameList[-1] in input_types

def createFolder(path):
    """
    Create a folder if it doesn't exist
    :param path: full path of creating directory
    :return:
    """
    if not exists(path):
        os.makedirs(path)
