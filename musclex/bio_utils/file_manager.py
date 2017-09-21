__author__ = 'Jiranun.J'

import os
from os.path import split, exists


def getImgFiles(fullname):
    """
    Get directory, all image file names in the same directory and current file index
    :param fullname: full name of the file including directory i.e. /aaa/bbb/ccc/ddd.tif (str)
    :return: directory (str), list of image file names, and current index i.e /aaa/bbb/ccc, ["ddd.tif","eee.tif"], 0
    """

    dir_path, filename = split(str(fullname)) # split directory and file name from full file name
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
    Combile a path and file name to get full file name
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
    # imgList = ['bmp','jpg','tif','tiff','png','jpeg']
    imgList = ['tif', 'tiff']
    nameList = fileName.split('.')
    if nameList[-1] in imgList:
        return True
    else:
        return False


def createFolder(path):
    """
    Create a folder if it doesn't exist
    :param path: full path of creating directory
    :return:
    """
    if not exists(path):
        os.makedirs(path)

def getStyleSheet():
    """
    Get style sheet from stylesheet.txt
    :return: styesheet (str) or empty string if it's not available
    """
    if exists("/stylesheet.txt"):
        sfile = "/stylesheet.txt"
    elif exists("stylesheet.txt"):
        sfile = "stylesheet.txt"
    else:
        return ""
    style_sheet = ""
    f = open(sfile, 'r')
    for line in f:
        if "#EOF" in line:
            break
        if "##" in line:
            continue
        line = line.rstrip('\n')
        style_sheet += line
    return style_sheet