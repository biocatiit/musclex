import sys
import json
import os
from os.path import splitext, join, exists
import traceback
import fabio
import pandas as pd
from PIL import Image
from musclex import __version__
try:
    from ..utils.file_manager import *
    from ..utils.image_processor import *
except: # for coverage
    from utils.file_manager import *
    from utils.image_processor import *
    
    

class AddIntensitiesExph:
    def __init__(self, path, frameNb):
        self.path = path
        self.frameNb = frameNb
        
        self.image_files = []
        self.added_images = []

        self.loadImages()
            
            
    def loadImages(self):
        self.image_files = sorted([f for f in os.listdir(self.path) if f.endswith(('tif', 'tiff'))])
        
        if len(self.image_files) % self.frameNb != 0:
            print(f"Warning: Total number of images ({len(self.image_files)}) is not a multiple of the group size ({self.frameNb}). Some images will be excluded.")
    
        image_groups = [self.image_files[i:i+self.frameNb] for i in range(0, len(self.image_files), self.frameNb)]
        self.added_images = []
        
        for group in image_groups:
            sum_img = 0
            for image in group:
                img = fabio.open(join(self.path, image)).data.astype(np.float32)
                if not isinstance(sum_img, int) and (img.shape[0] > sum_img.shape[0] or img.shape[1] > sum_img.shape[1]):
                    sum_img = resizeImage(sum_img, img.shape)
                elif not isinstance(sum_img, int):
                    img = resizeImage(img, sum_img.shape)
                sum_img += img
            self.added_images.append(sum_img)
        self.saveImages()
            
    def saveImages(self):
        folder_path = join(self.path, 'aise_results')
        createFolder(folder_path)
        for i, img in enumerate(self.added_images):
            file_name = "output_" + str(i) + ".tif"
            result_path = join(folder_path, file_name)
            fabio.tifimage.tifimage(data=img).write(result_path)
                
                
def resizeImage(img, res_size):
    """
    Resize the image.
    """
    if img.shape == res_size:
        return img
    print("Size mismatched, resizing image")
    h, b = img.shape
    resH, resB = res_size
    dH = resH - h
    dB = resB - b
    extraH = dH//2
    extraB = dB//2
    res_img = np.zeros((res_size))
    res_img[extraH:extraH+h, extraB:extraB+b] = img
    return res_img
        