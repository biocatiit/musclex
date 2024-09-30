import sys
import json
import os
from os.path import splitext, join, exists, dirname
import traceback
import collections
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
    def __init__(self, path, settings_path, type, mode):
        self.path = path
        self.settings_path = settings_path
        self.mode = mode
        self.type = type
        self.settings = None
        
        # Settings
        self.nbOfFrames = 2
        self.calSettings = None
        self.blankImageSettings = None
        
        
        self.image_files = []
        self.file_names = []
        self.file_name_groups = []
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
                
            if 'custom' in self.settings:
                print("handle custom")
            else:
                self.nbOfFrames = self.settings['nbOfFrames']
            
            
    def loadImages(self):
        if self.mode == 'aise':
            if self.type == 'folder':
                self.image_files = sorted([f for f in os.listdir(self.path) if f.endswith(('tif', 'tiff'))])
                self.file_name_groups = [[] for _ in range(len(self.image_files))]
            elif self.type == 'file':
                with fabio.open(self.path) as series:
                    for frame in series.frames():
                        self.image_files.append(ifHdfReadConvertless(self.path, frame.data).astype(np.float32))
                        namefile = os.path.split(frame.file_container.filename)[1].split('.')
                        temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                        self.file_names.append(temp_filename)
                self.file_name_groups = [self.file_names[i:i+self.nbOfFrames] for i in range(0, len(self.file_names), self.nbOfFrames)]
        
            if len(self.image_files) % self.nbOfFrames != 0:
                print(f"\nWarning: Total number of images ({len(self.image_files)}) is not a multiple of the group size ({self.nbOfFrames}). Some images will be excluded.")
        
            self.image_groups = [self.image_files[i:i+self.nbOfFrames] for i in range(0, len(self.image_files), self.nbOfFrames)]
            
            if self.type == 'folder':
                grps = self.image_groups
            else:
                grps = self.file_name_groups
            detail = "Available Groups : \n"
            if len(grps) >= 1:
                for (i, grp) in enumerate(grps):
                    detail += "Group "+ str(i+1)+ " : " + str(grp[0]) + " ... " + str(grp[-1]) + '\n'

            print(detail)
        else:
            self.folder_paths = []
            self.fileList = []
            self.orig_imgs = []
            self.ignore_files = ['_results', 'settings']
            self.numberToFilesMap = collections.defaultdict(list)
            self.numberToFilesMapPath = collections.defaultdict(list)
            self.isHdf5 = False
            for fname in os.listdir(self.path):
                isDataFile = True
                f = os.path.join(self.path, fname)
                if isImg(f):
                    self.folder_paths.append(fname)
                    i = -1
                    name = fname.split('.')[0]
                    number = name.split('_')[i]
                    if fname.split('.')[1] in ['h5', 'hdf5']:
                        self.isHdf5 = True
                        isDataFile = False
                    while not number.isnumeric():
                        i -= 1
                        number = name.split('_')[i]
                    if self.isHdf5:
                        for n in name.split('_'):
                            if n == 'data':
                                isDataFile = True
                    if isDataFile:
                        if self.isHdf5:
                            self.fileList.append(f)
                            with fabio.open(f) as series:
                                for frame in series.frames():
                                    namefile = os.path.split(frame.file_container.filename)[1].split('.')
                                    temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                                    self.numberToFilesMap[frame.index].append(temp_filename)
                                    self.numberToFilesMapPath[frame.index].append(ifHdfReadConvertless(self.path, frame.data).astype(np.float32))
                        else:
                            self.numberToFilesMap[int(number) - 1].append(f)
                        self.numberToFilesMap[int(number) - 1].sort()
                else:
                    if not any(ignore_str in fname for ignore_str in self.ignore_files):
                        temp_path = os.path.join(self.path, fname)
                        self.folder_paths.append(temp_path)
                        if os.path.isdir(temp_path):
                            i = 0
                            for fname2 in os.listdir(temp_path):
                                if isImg(fname2):
                                    self.numberToFilesMap[i].append(fname2)
                                    self.numberToFilesMapPath[i].append(os.path.join(temp_path, fname2))
                                i += 1
        
    def startProcessing(self):
        
        if self.mode == 'aise':
            if self.type == 'folder':
                output_dir = join(self.path, 'aise_results')
            else:
                output_dir = join(dirname(self.path), 'aise_results')
            createFolder(output_dir)
            l = multiprocessing.Lock()
            
            with multiprocessing.Pool(initializer=init, initargs=(l,) , processes=multiprocessing.cpu_count()) as pool:
                pool.map(addImageChunk, [(group, self.settings, self.path, output_dir, i, self.file_name_groups[i], self.mode) for i, group in enumerate(self.image_groups)])
            
            # Sort the CSV when its done
            
            file_path = join(output_dir, 'intensities.csv')
            if os.path.exists(file_path):
                df = pd.read_csv(file_path)
                df = df.sort_values(by=['Filename'])
                df.to_csv(file_path, index=False)
                
            print("Processing done. Results saved in aise_results")
        else:
            output_dir = join(self.path, 'aime_results')
            createFolder(output_dir)
            l = multiprocessing.Lock()
            if self.isHdf5:
                with multiprocessing.Pool(initializer=init, initargs=(l,) , processes=multiprocessing.cpu_count()) as pool:
                    pool.map(addImageChunk, [ (group, self.settings, self.path, output_dir, i, [5,6,7,8], self.mode) for (i, group) in self.numberToFilesMapPath.items()])
            else:
                with multiprocessing.Pool(initializer=init, initargs=(l,) , processes=multiprocessing.cpu_count()) as pool:
                    pool.map(addImageChunk, [ (group, self.settings, self.path, output_dir, i, [], self.mode) for (i, group) in self.numberToFilesMapPath.items()])

            print("Processing done. Results saved in aime_results")
            
def addImageChunk(args):
    chunk, settings, path, output, index, filenames, mode = args
    
    if len(chunk) > 0:
        if mode == 'aise':
            if len(filenames) > 0:
                first = filenames[0]
                last = filenames[-1]
            else:
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
        else:
            filename = "result_group_" + str(index + 1).zfill(5) + ".tif"

        sum_img = generateSumIng(chunk, settings, path, filenames, mode)
        orig_img = None
        mask = None 

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
        
        
        if mode == 'aise':
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
        
def generateSumIng(chunk, settings, path, filenames, mode):
    sum_img = 0
    img = 0
    if settings['avgInsteadOfSum'] == True:
        images = []
        if len(filenames) > 0:
            images = chunk
        else:
            for img_path in chunk:
                img = fabio.open(join(path, img_path)).data.astype(np.float32)
                images.append(img)
        
        if 'detector' in settings:
            sum_img = averageImages(images, preprocessed=True, man_det=settings['detector'])
        else:
            sum_img = averageImages(images, preprocessed=True)
        
    else:
        for img_path in chunk:
            if len(filenames) > 0:
                img = img_path
            else:
                if mode == 'aise':
                    img = fabio.open(join(path, img_path)).data.astype(np.float32)
                elif mode == 'aime':
                    img = fabio.open(img_path).data.astype(np.float32)
            
            if not isinstance(sum_img, int) and (img.shape[0] > sum_img.shape[0] or img.shape[1] > sum_img.shape[1]):
                sum_img = resizeImage(sum_img, img.shape)
            elif not isinstance(sum_img, int):
                img = resizeImage(img, sum_img.shape)
            sum_img += img
            
    return sum_img
            
        
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
        