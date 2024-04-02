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

### ORIGINAL PYTHON SCRIPT TO COMPRESS AND DECOMPRESS TIFF FILES

import argparse
import os
import fabio
import numpy as np
from PIL import Image

def decompress_tiff_files(fn, compress):
    """
    Decompress/compress tiff files.
    :param im, path, prefix:
    :return: -
    """
    with Image.open(fn) as im:
        
        name,ext = os.path.splitext(fn)
        
        if compress:
            print('Compressing TIFF Files...')
            new_fn = f"{name}_compressed{ext}"
            im.save(new_fn, compression='tiff_lzw')
        else:
            print('Decompressing TIFF Files...')
            data = np.array(im)
            new_fn = f"{name}_uncompressed{ext}"
            fabio.tifimage.tifimage(data=data).write(new_fn)
        # os.remove(fn)
        print('Completed')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='The script will decompress the TIF files from given compressed TIF files or folders. Reciprocally, this script can compress TIF files or folders to tiff_lzw. \
            All the images converted have the same name as the original images, which means the script overwrites the data.')
    parser.add_argument('-i', metavar='file', help='Path to the TIFF files', nargs='*')
    parser.add_argument('-f', metavar='folder', help='Path to the TIFF folders', nargs='*')
    parser.add_argument('-z', action='store_true', help='If this option is set, the script will generate a compressed version of the TIF images. \
                        Else, it will generate a decompressed version of it.')

    args = parser.parse_args()

    compress = args.z
    filename = args.i
    foldername = args.f
    if foldername:
        for folder in foldername:
            list_files = sorted(os.listdir(folder))
            for file in list_files:
                if os.path.splitext(file)[1] in ('.tiff', '.tif'):
                    f = os.path.join(folder, file)
                    print(f)
                    decompress_tiff_files(f, compress)
    elif filename:
        for f in filename:
            print(f)
            decompress_tiff_files(f, compress)
    else:
        print(parser.format_help())
