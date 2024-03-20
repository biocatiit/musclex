import glob
import os
import numpy as np
from scipy import stats
from .image_processor import *
from matplotlib import pyplot as plt

class Item:
    def __init__(self, image, center, angle, image_name):
        self.image_name = image_name
        self.image = image  # Load image and convert to float32
        self.center = center  # center is a tuple (x, y)
        self.angle = angle

    def __repr__(self):
        return f"Item(image_name={self.image_name.split('/')[-1]}, image_shape={self.image.shape}, center={self.center}, angle={self.angle})"
    def getName(self):
        return self.image_name

items = []
max_intensity = 255
global_distance_mode = 5
coefficients = {
        'image': 0.33,
        'center': 0.33,
        'angle': 0.33
    }

def addImages(image, center, angle, name):
    items.append(Item(image, center, angle, name))

def detectImages (folder_path, max_intensity, distance_mode, coefficients):
    global global_distance_mode
    
    max_intensity = max_intensity
    global_distance_mode = distance_mode
    coefficients = coefficients

    print("Processing with the following parameters:")
    print("Max intensity:", max_intensity)
    print("Distance mode:", global_distance_mode)
    print("Coefficients:", coefficients)

    # tiff_files = glob.glob(os.path.join(folder_path, '*.tif'))

    # for i, tiff_file in enumerate(tiff_files):
    #     image = fabio.open(tiff_file).data.astype(np.float32)
    #     center = getCenter(image)
    #     angle = getRotationAngle(image, center)
    #     items.append(Item(image, center, angle, tiff_file))
    # Distance mode (adapted for images)
    # 1: image; 2: center; 3: center + angle; 4: center + image; 5: center + angle + image
    # User-supplied coefficients for different aspects (image, center, angle

    total_coefficient = sum(coefficients.values())
    coefficients = {k: v / total_coefficient for k, v in coefficients.items()}
    center_medians = np.median([np.linalg.norm(np.array(item.center)) for item in items])
    angle_medians = np.median([item.angle for item in items])

    inconsistent_items = find_inconsistencies(items, center_medians, angle_medians)
    print("Inconsistent items:", inconsistent_items)
    items.clear()
    return inconsistent_items


# Define distance measurement functions
def measure_distance(item1, item2, center_median, angle_median):
    # Compute the average absolute difference between images (as a distance)
    if global_distance_mode in [1, 4, 5]:
        image_difference = np.abs(item1.image - item2.image) / max_intensity
        image_distance = np.mean(image_difference) * coefficients['image']
    else:
        image_distance = 0

    if global_distance_mode in [2, 3, 4, 5]:
        center_difference = np.linalg.norm(np.array(item1.center) - np.array(item2.center))
        center_distance = (center_difference / center_median) * coefficients['center']
    else:
        center_distance = 0

    if global_distance_mode in [3, 5]:
        angle_difference = abs(item1.angle - item2.angle)
        angle_distance = (angle_difference / angle_median) * coefficients['angle']
    else:
        angle_distance = 0

    return image_distance + center_distance + angle_distance

def find_inconsistencies(items, center_median, angle_median):
    # Calculate distance scores for each pair of subsequent items
    print(global_distance_mode)
    distances = [measure_distance(items[i], items[i+1], center_median, angle_median) for i in range(len(items) - 1)]
    distances.append(measure_distance(items[-1], items[0], center_median, angle_median)) # compare last image and first image
    
    distance_pairs = [[i, i+1] for i in range(len(items))]
    distance_pairs.append([len(items) - 1, 0])
    print(distance_pairs)
    
    # Find the 80th percentile of the distances
    threshold = np.percentile(distances, 80)
    #threshold = np.max(threshold, 0.1)
    print(distances)
    
    # Classify items based on the threshold
    inconsistent_items_indices = [i for i in range(len(distances)) if distances[i] > threshold]
    
    inconsistent_items_index = []
    
    for i in range(len(inconsistent_items_indices)):
        if i+1 < len(inconsistent_items_indices):
            set1 = set(distance_pairs[inconsistent_items_indices[i]])
            set2 = set(distance_pairs[inconsistent_items_indices[i+1]])
            common_values = list(set1.intersection(set2))
            inconsistent_items_index.append(common_values[0])    
            

    # Output inconsistent items
    inconsistent_items = [items[i] for i in inconsistent_items_index]
    
    # Also include the next item in the sequence if the last comparison was inconsistent
    if len(distances) in inconsistent_items_indices:
        inconsistent_items.append(items[-1])

    return inconsistent_items


