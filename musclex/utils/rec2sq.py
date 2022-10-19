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

import argparse
import os
import fabio
import numpy as np
from skimage.feature import peak_local_max

def isDir(pathname):
    """
    Return a boolean depending on if the path is a directory or not
    """
    if os.path.isdir(pathname):
        return True
    else:
        return False

def detectCenter(image):
    """
    Detect the center and return it if it exists
    """
    coordinates = peak_local_max(image, min_distance=20,threshold_rel=0.7)
    if len(coordinates) > 0:
        center = coordinates[0]
        return center
    else:
        return None

def isImg(fileName):
    """
    Check if a file name is an image file
    :param fileName: (str)
    :return: True or False
    """
    input_types = ['bmp','jpg','tif','tiff','png','jpeg']
    nameList = fileName.split('.')
    return nameList[-1] in input_types

def fullPath(filePath, fileName):
    """
    Return the full path by joining the filepath and the filename
    :param filepath, fileName:
    :return: full file path
    """
    if filePath[-1] == '/':
        return filePath+fileName
    else:
        return filePath+"/"+fileName

def getImgFiles(input):
    """
    Give the list of images in a folder
    :param input:
    :return: image list
    """
    dir_path=str(input)
    dir_path = str(dir_path)
    fileList = os.listdir(dir_path)
    imgList = []
    for f in fileList:
        full_file_name = fullPath(dir_path, f)
        if isImg(full_file_name):
            imgList.append(f)

    imgList.sort()
    return imgList

def combine_image(image,direction):
    """
    Combine image
    """
    size=max(image.shape[0],image.shape[1])
    newimage=np.zeros((size,size))
    if direction == 1:
        newimage[:image.shape[0],:]=image
    elif direction==2:
        newimage[size-image.shape[0]:,:]=image
    elif direction==3:
        newimage[:,:image.shape[1]]=image
    elif direction==4:
        newimage[:,size-image.shape[1]:]=image
    return newimage

def main(args):
    """
    Main function
    """
    input=args.input
    output=args.output
    if not isDir(input):
        print("input folder is invalid")
        return
    try:
        os.makedirs(output, exist_ok = True)
        print(f"Directory '{output}' created successfully")
    except OSError:
        print("Directory '%s' can not be created")
    imgList=getImgFiles(input)
    if len(imgList)==0:
        print("Input folder doesn't have image files")
        return
    f=os.path.join(input,imgList[0])

    image=fabio.open(f).data

    row=image.shape[0]
    col=image.shape[1]
    center=detectCenter(image)

    if center is None:
        direction=1
    else:

        if row>col:
            if center[1]>col/2:
                direction=3
            else:
                direction=4
        else:
            if center[0]>row/2:
                direction=1
            else:
                direction=2
    for f in imgList:
        filepath=os.path.join(input,f)
        image=fabio.open(filepath).data
        if not np.any(image):
            pass
        else:
            newimage=combine_image(image,direction)
            outputfile=os.path.join(output,"squared_"+f)
            fabio.tifimage.tifimage(data=newimage).write(outputfile)

if __name__=='__main__':
    parser=argparse.ArgumentParser(description='convert a rectangle image to square image')
    parser.add_argument('--input',help="Please type in input folder path",required=True)
    parser.add_argument('--output',help="Please type in output folder path",required=True)
    args=parser.parse_args()
    main(args)
