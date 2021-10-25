import argparse

from PIL import Image
from PIL.TiffTags import TAGS


def print_tiff_meta(filename):
    with Image.open(filename) as img:
        for key in img.tag:
            print(TAGS[key], img.tag[key])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Prints the metadata of the given TIFF file')
    parser.add_argument('-t', metavar='tiff', help='Path to the TIFF file')
    args = parser.parse_args()

    tiff_filename = args.t
    if not tiff_filename:
        print(parser.format_help())
    else:
        print_tiff_meta(tiff_filename)
