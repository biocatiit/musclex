import sys
import json
import os
from os.path import splitext, join, exists
import traceback
import fabio
import multiprocessing
import csv
from datetime import datetime
import pandas as pd
from PIL import Image
from musclex import __version__
try:
    from ..utils.file_manager import *
    from ..utils.image_processor import *
except: # for coverage
    from utils.file_manager import *
    from utils.image_processor import *
    

def init(l):
    global lock
    lock = l    
    

class AddIntensitiesExph:
    def __init__(self, path, settings_path, mode):
        self.path = path
        self.settings_path = settings_path
        self.mode = mode
        self.settings = None
        
        # Settings
        self.nbOfFrames = 2
        self.calSettings = None
        self.blankImageSettings = None
        
        
        self.image_files = []
        self.added_images = []

        self.loadSettings()
        self.loadImages()
        self.startProcessing()
            
            
    def loadSettings(self):
        if self.settings_path == 'default':
            self.nbOfFrames = 2
        else:
            with open(self.settings_path, 'r') as f:
                self.settings = json.load(f)
            if 'calSettings' in self.settings:
                self.calSettings = self.settings['calSettings']
            if 'blankImageSettings' in self.settings:
                self.blankImageSettings = self.settings['blankImageSettings']
            self.nbOfFrames = self.settings['nbOfFrames']
            
            
    def loadImages(self):
        if self.mode == 'folder':
            self.image_files = sorted([f for f in os.listdir(self.path) if f.endswith(('tif', 'tiff'))])
        
        if len(self.image_files) % self.nbOfFrames != 0:
            print(f"Warning: Total number of images ({len(self.image_files)}) is not a multiple of the group size ({self.nbOfFrames}). Some images will be excluded.")
    
        self.image_groups = [self.image_files[i:i+self.nbOfFrames] for i in range(0, len(self.image_files), self.nbOfFrames)]
        print(self.image_groups)
        # self.added_images = []
        
        # for group in image_groups:
        #     sum_img = 0
        #     for image in group:
        #         img = fabio.open(join(self.path, image)).data.astype(np.float32)
        #         if not isinstance(sum_img, int) and (img.shape[0] > sum_img.shape[0] or img.shape[1] > sum_img.shape[1]):
        #             sum_img = resizeImage(sum_img, img.shape)
        #         elif not isinstance(sum_img, int):
        #             img = resizeImage(img, sum_img.shape)
        #         sum_img += img
        #     self.added_images.append(sum_img)
        # self.saveImages()
        
    def startProcessing(self):
        output_dir = join(self.path, 'aise_results')
        createFolder(output_dir)
        l = multiprocessing.Lock()
        with multiprocessing.Pool(initializer=init, initargs=(l,) , processes=multiprocessing.cpu_count()) as pool:
            pool.map(addImageChunk, [(group, self.settings, self.path, output_dir, i) for i, group in enumerate(self.image_groups)])
        
        # Sort the CSV when its done
        
        file_path = join(output_dir, 'intensities.csv')
        if os.path.exists(file_path):
            df = pd.read_csv(file_path)
            df = df.sort_values(by=['Filename'])
            df.to_csv(file_path, index=False)
        
        
    def processImage(self):
        print("placeholder")
            
    def saveImages(self):
        folder_path = join(self.path, 'aise_results')
        createFolder(folder_path)
        for i, img in enumerate(self.added_images):
            file_name = "output_" + str(i) + ".tif"
            result_path = join(folder_path, file_name)
            
            tiff_image = fabio.tifimage.tifimage(data=img)
            tiff_image.write(result_path)
            
def addImageChunk(args):
    chunk, settings, path, output, index = args
    
    if len(chunk) > 0:
        first = chunk[0]
        last = chunk[-1]
        f_ind1 = first.rfind('_')
        f_ind2 = first.rfind('.')
        l_ind1 = last.rfind('_')
        l_ind2 = last.rfind('.')

        if f_ind1 == -1 or f_ind2 == -1 or l_ind1 == -1 or l_ind2 == -1 or first[:f_ind1] != last[:l_ind1]:
            filename = "group_"+str(index + 1).zfill(5)+'.tif'
        else:
            filename = first[:f_ind1] + "_"
            filename += first[f_ind1 + 1:f_ind2]
            filename += "_"
            filename += last[l_ind1 + 1:l_ind2]
            filename += '.tif'

        sum_img = 0
        orig_img = None
        mask = None
        
        if settings['avgInsteadOfSum'] == True:
            images = []
            for img_path in chunk:
                img = fabio.open(join(path, img_path)).data.astype(np.float32)
                images.append(img)
            
            if 'detector' in settings:
                sum_img = averageImages(images, preprocessed=True, man_det=settings['detector'])
            else:
                sum_img = averageImages(images, preprocessed=True)
            
        else:
            for img_path in chunk:
                img = fabio.open(join(path, img_path)).data.astype(np.float32)
                
                if not isinstance(sum_img, int) and (img.shape[0] > sum_img.shape[0] or img.shape[1] > sum_img.shape[1]):
                    sum_img = resizeImage(sum_img, img.shape)
                elif not isinstance(sum_img, int):
                    img = resizeImage(img, sum_img.shape)
                sum_img += img

        if 'blankImageSettings' in settings:
            if settings['blankImageSettings']['subtractBlank'] == True:
                raw_filepath = r"{}".format(settings['blankImageSettings']['path'])
                blank_image = fabio.open(raw_filepath).data
                weight = settings['blankImageSettings']['weight']
                sum_img = sum_img - weight * blank_image * len(chunk)
                if settings['blankImageSettings']['clampNegativeValues'] == True:
                    sum_img = np.clip(sum_img, 0, None) 
                
        if 'maskPath' in settings:
            mask_path = r"{}".format(settings['maskPath'])
            mask = fabio.open(mask_path).data
            orig_img = sum_img
            sum_img = sum_img * mask
            endpt = filename.rfind('.')
            avg_intensity = np.sum(orig_img) / np.sum(sum_img)
            masked_filename = filename[:endpt] + "_masked" + filename[endpt:]
        
        # Save the image
        if settings['compress'] == True:
            if 'maskPath' in settings:
                mask_tif_img = Image.fromarray(sum_img)
                mask_tif_img.save(os.path.join(output, masked_filename), compression='tiff_lzw')
                tif_img = Image.fromarray(orig_img)
                tif_img.save(os.path.join(output, filename), compression='tiff_lzw')
            else:
                tif_img = Image.fromarray(sum_img)
                tif_img.save(os.path.join(output, filename), compression='tiff_lzw')
        else:
            if 'maskPath' in settings:
                fabio.tifimage.tifimage(sum_img).write(os.path.join(output, masked_filename))
                fabio.tifimage.tifimage(orig_img).write(os.path.join(output, filename))
            else:
                fabio.tifimage.tifimage(sum_img).write(os.path.join(output, filename))
                
        # Generate CSV data
        nonmaskedPixels = 0
        orig_img_intensity = 0
        img_with_mask_intensity = 0
        weight = 0
            
        if orig_img is not None:
            orig_img_intensity = np.sum(orig_img)
        else:
            orig_img_intensity = np.sum(sum_img)
            
        if mask is not None:
            nonmaskedPixels = np.sum(mask == 1)
            img_with_mask_intensity = np.sum(sum_img)
        else:
            img_with_mask_intensity = orig_img_intensity
        
        dateString = datetime.now().strftime("%m/%d/%Y %H:%M:%S")
        
        # Masked Image Intensity (average) = Masked Image Intensity (Total) / Number of Pixels Not Masked
        average_mask = img_with_mask_intensity / nonmaskedPixels
        
        if 'blankImageSettings' in settings:
            weight = settings['blankImageSettings']['weight']
            
        if 'drawnMask' in settings:
            drawnMask = settings['drawnMask']
        else:
            drawnMask = False
            
        if 'computedMask' in settings:
            computedMask = settings['computedMask']
        else:
            computedMask = False
        
        data = [filename, dateString, orig_img_intensity, img_with_mask_intensity, nonmaskedPixels, average_mask, weight, settings['nbOfFrames'], drawnMask, computedMask]
        writeCSV(output, data)
        
def writeCSV(output_dir, data):
    file_path = join(output_dir, 'intensities.csv')
    # Write header line if file doesn't exist
    with lock:
        if not os.path.exists(file_path):
            with open(file_path, 'w') as f:
                writer = csv.writer(f)
                writer.writerow(['Filename', 'Date', 'Original Image Intensity (Total)', 'Masked Image Intensity (Total)', 'Number of Pixels Not Masked', 'Masked Image Intensity (Average)', 'Blank Image Weight', 'Binning Factor', 'Drawn Mask', 'Computed Mask'])
                writer.writerow(data)
        else:
            with open(file_path, 'a') as f:
                writer = csv.writer(f)
                writer.writerow(data)
            

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
        