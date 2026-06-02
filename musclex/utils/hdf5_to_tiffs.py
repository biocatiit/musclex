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
import glob
import fabio
import numpy as np

def log_progress(progress, total):
    """
    Print the progress in the terminal
    :param progress, total:
    :return: -
    """
    per = int(progress * 100 / total)
    print('\r[{1:>3}%  {0:40}]'.format('#' * int(40*per/100), per), end='')
    if per >= 100:
        print(' [DONE]')

def generate_tiff_files(fn, path, prefix, compress):
    """
    Generate tiff files from a hdf file.
    :param fn, metadata, path, prefix:
    :return: -
    """
    print('Generating TIFF Files...')
    with fabio.open(fn) as fabio_img:
        # create_tiff(fabio_img.data, metadata, path, prefix, 1)
        create_tiff(fabio_img, path, prefix, 1, compress)
        if fabio_img.nframes > 1:
            for i in range(2, fabio_img.nframes + 1):
                fabio_img = fabio_img.next()
                # create_tiff(fabio_img.data, metadata, path, prefix, i)
                create_tiff(fabio_img, path, prefix, i, compress)
                log_progress(i, fabio_img.nframes)
    print('Completed')

def create_tiff(img_data, path, prefix, serial, compress):
    """
    Create a tiff file from a hdf file.
    :param img_data, metadata, path, prefix, serial:
    :return: -
    """
    tif_file_name = path + os.sep + prefix + '_{:04d}'.format(serial) + '.tif'
    cmp_tif_file_name = path + os.sep + prefix + '_{:04d}'.format(serial) + '_cmp' + '.tif'
    # extra_tags = [("ImageDescription", 's', 0, metadata, True)]
    # tifffile.imsave(tif_file_name, img_data, extratags=extra_tags)
    data = img_data.data.astype(np.int32)
    data[data==4294967295] = -1
    if compress:
        from PIL import Image
        tif_img = Image.fromarray(data)
        tif_img.save(cmp_tif_file_name, compression='tiff_lzw', exif=img_data.getheader())
    else:
        tif_img = fabio.pilatusimage.pilatusimage(data=data, header=img_data.getheader())
        tif_img.write(tif_file_name)

def read_meta_data(meta_fn):
    """
    Read the meta data.
    :param meta_fn: filename where the metadata is stored
    :return: metadata
    """
    with open(meta_fn) as meta:
        return meta.read()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='The script will generate the tiff files from the given hdf5 file. Metadata will be read from the given metadata file and added as an ImageDescription tag in all the tiff files.')
    parser.add_argument('-h5', metavar='hdf5', help='Path to the Hdf5 file', nargs='*')
    parser.add_argument('-m', metavar='metadata', help='Path to the metadata text file')
    parser.add_argument('-z', action='store_true', help='Generate a compressed version of the TIF images')

    args = parser.parse_args()

    compress = args.z
    h5_filename = args.h5
    if not h5_filename:
        print(parser.format_help())
    else:
        #files = glob.glob(h5_filename)
        for f in h5_filename:
            print(f)
            path = os.path.dirname(os.path.abspath(f))
            prefix = os.path.basename(f).rsplit('.', 1)[0]
            # metadata = ''
            # if args.m:
            #     metadata = read_meta_data(args.m)
            
            generate_tiff_files(f, path, prefix, compress)
