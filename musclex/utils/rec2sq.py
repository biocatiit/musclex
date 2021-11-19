import argparse
import os
import fabio
from os.path import split, exists, join
from skimage.feature import peak_local_max
import numpy as np


def isDir(pathname):
    if os.path.isdir(pathname):
        return True
    else:
        return False
def detectCenter(image):
    coordinates = peak_local_max(image, min_distance=20,threshold_rel=0.7)
    if len(coordinates)>0:
        center=coordinates[0]
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
    if nameList[-1] in input_types:
        return True
    else:
        return False
def fullPath(filePath, fileName):
   
    if filePath[-1] == '/':
        return filePath+fileName
    else:
        return filePath+"/"+fileName

def getImgFiles(input):
  

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
    size=max(image.shape[0],image.shape[1])
    newimage=np.zeros((size,size))
    if(direction==1):
        newimage[:image.shape[0],:]=image
    elif direction==2:
        newimage[size-image.shape[0]:,:]=image
    elif direction==3:
        newimage[:,:image.shape[1]]=image
    elif direction==4:
        newimage[:,size-image.shape[1]:]=image
    return newimage
        






def main(args):
    input=args.input
    output=args.output
    if not isDir(input):
        print("input folder is invalid")
        return
    try:
        os.makedirs(output, exist_ok = True)
        print("Directory '%s' created successfully" %output)
    except OSError as error:
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

