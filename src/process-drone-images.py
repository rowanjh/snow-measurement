#!/usr/bin/env python
# ~~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' Purpose: 
#'      This script is designed to be called from the R script 
#'      `src/process-drone-images.R` using reticulate.
#'      It creates a set of function that process DNG photographs to extract 
#'      % snow cover and to produce rescaled snow masks.
#'      
#'      Python virtual environment setup and more information given in 
#'      `src/process-drone-images.R`
#' 
# ~~~~~~~~~~~~~~~ Load modules & Initialization ~~~~~~~~~~~~~~~~~~~~~~~~----
import os
import csv
import re
import rawpy
import exifread
import numpy as np
import matplotlib.pyplot as plt
from pyproj import Geod
from datetime import datetime
from skimage.io import imsave, imshow
from scipy.signal import argrelmin
from skimage.transform import rescale


def process_file(f, point_coords, mask_out_dir, scales = [0.03]):
    """Process a single drone photograph 
        
    Estimate %cover for, determine the location of, and export a downscaled snow 
    mask for a drone photograph
    
    Args:
        f (str):
            File path for the drone image to be processed
        point_coords (list of dict):
            A list of dictionaries giving coordinates of 38 sampling points, with 
            keys point_id, x (i.e. longitude), y (i.e. latitude). Coordinates are 
            given in cordinate reference system espg:4326 (WGS84)
        mask_out_dir (str):
            Directory in which to save masks files, or None
        scales (list of float):
            Scaling factor for mask output. Should be between 0 and 1. The 
            default 0.03 upscales such that each pixel is approximately 0.5m x 
            0.5m in size
    
    Returns:
        A dict containing point id, latitude and longitude, image capture time,
        %cover, snow classification threshold used, and source filename for the
        processed drone photograph
    
    Notes:
        To estimate percent snow cover, a snow cover threshold is estimated 
        as described in `get_snow_threshold`. The proportion of pixels with 
        a value on the blue channel higher than this threshold gives % cover.
        
        The photograph is assigned to one of the 38 drone sampling points by 
        cross-referencing photograph location from the image metadata against 
        the set of 38 drone sampling locations, given in the file
        `./data/drone-sampling-coords/sampling-grid-coordinates.csv`
        
    """
    # Load DNG image as numpy array
    with rawpy.imread(f) as raw:
        img = raw.postprocess()

    # Compute threshold for snow cover from full image
    thresh, _, _, _ = get_snow_threshold(img)

    # Compute percent cover
    cover = np.mean(img[:,:,2] > thresh)

    # Find point id and dt image was taken
    lat_lon, img_dt = get_img_info(f)
    point_id = get_point_id(lat_lon, point_coords)
    img_dt = datetime.strptime(img_dt, "%Y:%m:%d %H:%M:%S")

    # Export a downsized snow-cover mask for each level of rescaling
    stem = f'pt{point_id}_{img_dt.strftime("%Y-%m-%d_%H%M")}'
    for s in scales:
        mask = rescale(img[:,:,2], s, anti_aliasing=True) * 255
        mask = (mask > thresh) * 255
        mask = mask.astype('uint8')
        imsave(os.path.join(mask_out_dir, f'{stem}_scale_{s}.png'), mask, 
               check_contrast=False)
        
    # Return as dictionary
    return {'point_id': point_id, 'lat_lon': lat_lon, 'img_dt': img_dt, 
            'cover': cover, 'thresh': thresh, 'file': f}


def get_snow_threshold(img, min_thresh = 90, nbin = 51, smooth = 5):
    """ Compute the snow threshold for a drone image
    
    The snow threshold is a value between 0-255. Any pixels with a greater value
    than the threshold in the blue channel will be classified as snow-covered.

    Args:
        img (ndarray): 
            input image to find the snow threshold for, given as an 
            ndarray of size img_height x img_width x 3 channels.
        min_thresh (int): 
            the min allowed snow threshold. int in range (0,255)
        nbin: 
            number of histogram bins (51 bins gives binwidth 255 / 51 = 5)
        smoothing: 
            number of adjacent bins in the histogram to use for smoothing.
    
    Returns:
        thresh: An int between 0 and 255 giving the snow threshold for that image
        hist: the count in each histogram bin
        bins: histogram bin boundaries 
        smoothed: the count in each bin after smoothing
        
    Notes:
        The threshold is computed as follows: get a binned count of pixel values 
        (i.e. with 51 bins or a binwidth of 5). Smooth the binned counts with a 
        window size of 5 by default (i.e. average with the two neighbouring bins
        each side). Then get the first local minima above the minimum threshold 
        allowed (90). If no local minimum is found, then the minimum threshold is 
        used. The original algorithm uses a min threshold of 127, but 90 worked 
        better for this dataset.
    """
    # Get the blue pixels from image
    blue_px = img[:,:,2].flatten()
    
    # Bin pixel values into `nbin` bins,
    hist, bins = np.histogram(blue_px, bins = nbin, range = [0, 255])

    # Smooth the histogram by averaging adjacent bins
    smoothed = np.convolve(np.pad(hist,((smooth-1)//2, (smooth-1)//2), 'edge'), 
                           np.ones(smooth), 'valid') / smooth
    
    # Find local minima over the minimum threshold
    minima = bins[argrelmin(smoothed)]
    minima = minima[minima >= min_thresh]
    
    if len(minima) > 0:
        # Threshold is the first local minima over min_thresh
        thresh = minima[0]
    else:
        # If there is no minimum, use min_thresh as the threshold
        thresh = min_thresh

    return thresh, hist, bins, smoothed


def get_img_info(file):
    """ Get the GPS lat/long and datetime of an image from exif data

    Args:
        file (str): 
            the path to an image file with exif metadata

    Returns:
        coords (tuple): (latitude, longitude) from exif metadata
        datetime (str): EXIF DateTimeOriginal timestamp from exif metadata
    """
    # Read EXIF tags for file
    with open(file, 'rb') as f:
        tags = exifread.process_file(f, details = False)
    
    # Extract lat/lon
    lat = tags['GPS GPSLatitude'].values
    lon = tags['GPS GPSLongitude'].values
    
    # Convert coordinates to decimal degrees
    lat = lat[0] + lat[1] / 60 + float(lat[2]) / 3600 
    lon = lon[0] + lon[1] / 60 + float(lon[2]) / 3600 
    lon = lon * -1 # account for E/W

    # Get datetime of photo and reformat 
    if re.search('snowmelt-1(?![0-9])', file):
        datetime = '2022:05:29 14:30:00'
    else:
        datetime = tags['EXIF DateTimeOriginal'].values
        
    coords = (lat, lon)
    return coords, datetime


def load_pt_coords(csv_path):
    """
    Load the csv file containing the GPS coordinates of each sampling point. 
    
    Args:
        csv_path (str):
            path to the csv file containing GPS coordinates
    
    Returns: 
        point_coords:
            A list of dictionaries with keys `point_id`, `x` (longitude), and 
            `y` (latitude)
    """
    with open(csv_path) as f:
        r = csv.DictReader(f)
        point_coords = [{'pt_id': row['ID'], 'x': float(row['X']), 'y': float(row['Y'])} for row in r]
    return point_coords


def get_point_id(target, points):
    """
    Takes the lat/long values of a point (drone's position when a photograph
    was taken), and compare to the sampling grid to find the point that 
    corresponds to this location. Allow max ~30m of error (should be ~200m 
    between points))
    
    Args:
        target: a tuple (lat, lon) giving coordinates of a position (i.e. drone
            position when the photograph was taken that will be assigned a point_id
        points: a list of dictionaries representing points of the sampling grid. 
            Each sampling point is represented by one of the dictionaries.
            Each dict should contain the ID, lat (Y) and lon (X) values for a single
            point.
        
    Returns:
        int: the id number of this sampling location (e.g. the 25th point on the 
            sampling transect may have ID = 25)
    """
    # Get GPS points within ~20m of this lat_lon position
    geod = Geod(ellps='WGS84')
    
    # Get distances between this point and all other GPS points (in meters)
    dists= \
    [[pt['pt_id'], geod.line_length(lons = [pt['x'], target[1]], 
                                    lats = [pt['y'], target[0]])]
        for pt 
        in points]
    
    # return any points with distance of less than 30m from photograph location
    point_id = [d[0] for d in dists if d[1] < 30]

    # If a single point wasn't found, then break.
    if len(point_id) != 1:
        raise Exception("Point did not match a grid point, or matched multiple grid points.")

    return point_id.pop() # return the point_id


def plot_file(f, outpath):
    """ Plot a drone photograph with its histogram and snow-classified pixels

    Args:
        f (str):
            File path for the drone image to be processed
        outpath (str):
            Directory in which to save plot
            
    Returns:
        None
    
    
    """
    # Load DNG image as numpy array
    with rawpy.imread(f) as raw:
        img = raw.postprocess()

    # Get the threshold and histogram for that image
    thresh, counts, bins, smoothed = get_snow_threshold(img)

    # Plot the histogram with threshold
    cm = 1/2.54
    fig, axd = plt.subplot_mosaic([['l', 'm', 'r']],
                                figsize=(15*cm, 3.5*cm), layout="constrained")

    axd['r'].bar(bins[:-1], smoothed, width = 5)
    axd['r'].vlines(thresh, 0, max(smoothed), color = 'red')
    axd['r'].set_yticks([])
    axd['r'].set_xticks([])

    # Force histogram aspect ratio to match plots
    # square aspect ratio
    asp = np.diff(axd['r'].get_xlim())[0] / np.diff(axd['r'].get_ylim())[0]
    # rect aspect ratio
    asp = asp * (img.shape[0] / img.shape[1])
    axd['r'].set_aspect(asp)

    # Add original image and mask of snow-classified pixels to plot
    axd['l'].imshow(img)
    axd['m'].imshow((img[:,:,2] > thresh).astype(int),
                    vmin = 0, vmax = 1, cmap = 'binary_r')
    axd['l'].set_xticks([])
    axd['l'].set_yticks([])
    axd['m'].set_xticks([])
    axd['m'].set_yticks([])

    # add %cover to plot
    cover = np.mean(img[:,:,2] > thresh)*100
    axd['r'].annotate(f'{int(cover)}%', [0.05, 0.9], xycoords = 'axes fraction')

    # Save plot 
    plt.savefig(outpath)
    plt.close()
    
    return None
