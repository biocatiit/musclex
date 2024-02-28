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

import os
import numpy as np
import tifffile
import fabio
import argparse


def filter_and_sort_files(
    folder_path, file_extension, start_image_name, end_image_name, str_to_exclude
):
    """Filter and sort files by extension and exclude specified strings."""
    files = [file for file in os.listdir(folder_path) if file.endswith(file_extension)]
    files = sorted(files)

    try:
        start_index = files.index(start_image_name)
        end_index = files.index(end_image_name) + 1
    except ValueError:
        print("One or both of the provided image names are not in the folder.")
        raise ValueError("One or both of the provided image names are not in the folder.")

    files = files[start_index:end_index]

    if str_to_exclude:
        files = [
            file for file in files if not any(excl in file for excl in str_to_exclude)
        ]
        excluded_files = [
            file for file in files if any(excl in file for excl in str_to_exclude)
        ]
        if excluded_files:
            print(
                f"Excluding the following files due to the provided strings: {excluded_files}"
            )
        # if the last file is excluded, the end image name will be the new last file, same for start image name
        end_image_name = files[-1]
        start_image_name = files[0]

    return files, start_image_name, end_image_name


def sum_images(folder_path, files):
    """Sum up the images in the provided list of files."""
    if not files:
        print("No images to process.")
        return

    first_image_path = os.path.join(folder_path, files[0])
    strong_image = np.zeros_like(fabio.open(first_image_path).data, dtype=np.float32)
    print(f"Summing following images: {files}")
    for file in files:
        image_path = os.path.join(folder_path, file)
        image = fabio.open(image_path).data
        if image.shape != strong_image.shape:
            print(f"Excluding {file}: size differs from the first image.")
            continue
        image = image.astype(np.int32)
        image[image == 4294967295] = -1
        strong_image += image

    return strong_image


def save_strong_image(folder_path, start_image_name, end_image_name, strong_image):
    """Save the summed strong image."""
    output_folder = os.path.join(folder_path, "summed_files")
    os.makedirs(output_folder, exist_ok=True)
    start_image_name = start_image_name.split(".")[0]
    end_image_name = end_image_name.split(".")[0]
    output_file_name = f"strong_image_{start_image_name}_{end_image_name}.tif"
    output_file_path = os.path.join(output_folder, output_file_name)
    tifffile.imwrite(output_file_path, strong_image)
    print(f"Strong image saved to {output_file_path}")


def create_strong_image(
    folder_path, start_image_name, end_image_name, str_to_exclude=None
):
    """Create a strong image from TIFF or H5 files between start and end image names, excluding specified strings."""
    try:
        file_extension = ".tif" if start_image_name.endswith(".tif") else ".h5"
        files, new_start_img, new_end_img = filter_and_sort_files(
            folder_path,
            file_extension,
            start_image_name,
            end_image_name,
            str_to_exclude,
        )

        if not files:
            raise ValueError("No images to process.")

        print("Including the following files in the summed image:", files)
        strong_image = sum_images(folder_path, files)
        if strong_image is not None:
            save_strong_image(folder_path, new_start_img, new_end_img, strong_image)
    except Exception as e:
        raise e     


def main():
    parser = argparse.ArgumentParser(
        description="Create a strong image from a series of images."
    )
    parser.add_argument("--folder", help="Path to the folder containing the images.")
    parser.add_argument("--start", help="Name of the first image.")
    parser.add_argument("--end", help="Name of the last image.")
    parser.add_argument(
        "--exclude", help="List of strings to exclude from the summed image.", nargs="*"
    )

    args = parser.parse_args()
    if args.folder and args.start and args.end:
        create_strong_image(args.folder, args.start, args.end, args.exclude)
    else:
        print("Please provide the folder path, and start and end image names.")


if __name__ == "__main__":
    main()
