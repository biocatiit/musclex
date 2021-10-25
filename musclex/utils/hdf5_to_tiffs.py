import argparse
import os

import fabio
import tifffile


def log_progress(progress, total):
    per = int(progress * 100 / total)
    print('\r[{1:>2}%  {0:100}]'.format('#' * per, per), end='')
    if per >= 100:
        print(' [DONE]')


def generate_tiff_files(fn, metadata, path, prefix):
    print('Generating TIFF Files...')
    with fabio.open(fn) as fabio_img:

        create_tiff(fabio_img.data, metadata, path, prefix, 1)

        if fabio_img.nframes > 1:
            for i in range(2, fabio_img.nframes + 1):
                fabio_img = fabio_img.next()
                create_tiff(fabio_img.data, metadata, path, prefix, i)
                log_progress(i, fabio_img.nframes)

    print('Completed')


def create_tiff(img_data, metadata, path, prefix, serial):
    tif_file_name = path + os.sep + prefix + '_{:04d}'.format(serial) + '.tif'
    extra_tags = [("ImageDescription", 's', 0, metadata, True)]
    tifffile.imsave(tif_file_name, img_data, extratags=extra_tags)


def read_meta_data(meta_fn):
    with open(meta_fn) as meta:
        return meta.read()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='The script will generate the tiff files from the given hdf5 file. Metadata will be read from the '
                    'given metadata file and added as an ImageDescription tag in all the tiff files.')
    parser.add_argument('-h5', metavar='hdf5', help='Path to the Hdf5 file')
    parser.add_argument('-m', metavar='metadata', help='Path to the metadata text file')
    args = parser.parse_args()

    h5_filename = args.h5
    if not h5_filename:
        print(parser.format_help())
    else:
        path = os.path.dirname(h5_filename)
        prefix = os.path.basename(h5_filename).rsplit('.', 1)[0]
        metadata = ''
        if args.m:
            metadata = read_meta_data(args.m)

        generate_tiff_files(h5_filename, metadata, path, prefix)
