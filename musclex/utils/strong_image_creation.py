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
import sys
from PySide6.QtWidgets import (
    QApplication,
    QWidget,
    QVBoxLayout,
    QPushButton,
    QLabel,
    QLineEdit,
    QFileDialog,
    QMessageBox,
    QHBoxLayout,
    QRadioButton,
    QDialog,
    QDialogButtonBox
)
from PySide6.QtCore import Qt
from PySide6.QtWidgets import QHBoxLayout


class SumFramesGUI(QWidget):
    def __init__(self):
        super().__init__()
        self.title = "Sum Frames Within File"
        self.initUI()

    def initUI(self):
        self.setWindowTitle(self.title)
        self.setGeometry(100, 100, 400, 200)

        layout = QVBoxLayout()

        self.fileInput = QLineEdit()
        self.fileButton = QPushButton("Browse File")
        self.fileButton.clicked.connect(self.openFileDialog)
        self.startFrameInput = QLineEdit()
        self.endFrameInput = QLineEdit()
        self.sumFramesButton = QPushButton("Sum Frames")
        self.sumFramesButton.clicked.connect(self.onSubmit)

        layout.addWidget(QLabel("File Path:"))
        layout.addWidget(self.fileInput)
        layout.addWidget(self.fileButton)
        layout.addWidget(QLabel("Start Frame:"))
        layout.addWidget(self.startFrameInput)
        layout.addWidget(QLabel("End Frame:"))
        layout.addWidget(self.endFrameInput)
        layout.addWidget(self.sumFramesButton)

        self.setLayout(layout)

    def openFileDialog(self):
        fileName, _ = QFileDialog.getOpenFileName(
            self, "Open File", "", "Image Files (*.tif *.tiff *.h5)"
        )
        if fileName:
            self.fileInput.setText(fileName)

    def onSubmit(self):
        file_path = self.fileInput.text()
        start_frame = int(self.startFrameInput.text())
        end_frame = int(self.endFrameInput.text())
        output_folder = os.path.join(os.path.dirname(file_path), "summed_files")
        
        create_strong_image_from_frames(file_path, start_frame, end_frame)
        
        print(
            f"Summing frames {start_frame} to {end_frame} in {file_path}, saving to {output_folder}"
        )
        QMessageBox.information(self, "Success", "Frames have been summed and saved.")


class SumImagesGUI(QWidget):
    def __init__(self):
        super().__init__()
        self.title = "Sum Images Within Folder"
        self.initUI()

    def initUI(self):
        self.setWindowTitle(self.title)
        layout = QVBoxLayout()

        # Image selection input
        self.imagesLabel = QLabel("Select Start and End Images, holding Ctrl to select multiple images:")
        layout.addWidget(self.imagesLabel)
        self.imagesButton = QPushButton("Browse Images", self)
        self.imagesButton.clicked.connect(self.openImagesDialog)
        layout.addWidget(self.imagesButton)

        orLayout = QHBoxLayout()
        self.orLabel = QLabel("or")
        orLayout.addWidget(self.orLabel, 0, Qt.AlignHCenter)
        layout.addLayout(orLayout)

        self.manualSelectionLabel = QLabel("Manually Input Start and End Image Path:")
        layout.addWidget(self.manualSelectionLabel)

        self.startImageLabel = QLabel("Start Image Path:")
        self.startImageInput = QLineEdit(self)
        layout.addWidget(self.startImageLabel)
        layout.addWidget(self.startImageInput)

        self.endImageLabel = QLabel("End Image Path:")
        self.endImageInput = QLineEdit(self)
        layout.addWidget(self.endImageLabel)
        layout.addWidget(self.endImageInput)

        # Strings to exclude input
        self.excludeLabel = QLabel("Strings in filenames to Exclude (comma-separated):")
        layout.addWidget(self.excludeLabel)
        self.excludeInput = QLineEdit(self)
        layout.addWidget(self.excludeInput)

        # Submit button
        self.submitButton = QPushButton("Create Strong Image", self)
        self.submitButton.clicked.connect(self.onSubmit)
        layout.addWidget(self.submitButton)

        self.setLayout(layout)

    def openFolderDialog(self):
        options = QFileDialog.Options()
        folder = QFileDialog.getExistingDirectory(
            self, "Select Directory", options=options
        )
        if folder:
            self.folderInput.setText(folder)

    def openImagesDialog(self):
        options = QFileDialog.Options()
        files, _ = QFileDialog.getOpenFileNames(
            self, "Select Images", "", "Images (*.tif *.tiff *.h5)", options=options
        )
        if files:
            start_image = files[0]
            end_image = files[-1]
            
            self.startImageInput.setText(start_image)
            self.endImageInput.setText(end_image)

    def onSubmit(self):
        start_image_path = self.startImageInput.text()
        end_image_path = self.endImageInput.text()
        str_to_exclude = (
            [x.strip() for x in self.excludeInput.text().split(",")]
            if self.excludeInput.text()
            else []
        )
        

        try:
            folder_path = os.path.dirname(start_image_path)
            start_image_name = os.path.basename(start_image_path)
            end_image_name = os.path.basename(end_image_path)
            
            print(f"Summing images from {start_image_name} to {end_image_name} in {folder_path}")
            create_strong_image(
                folder_path, start_image_name, end_image_name, str_to_exclude
            )
            QMessageBox.information(
                self, "Success", "The strong image has been successfully created."
            )
        except Exception as e:
            QMessageBox.critical(self, "Error", f"An error occurred: {str(e)}")


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
    for file in files:
        image_path = os.path.join(folder_path, file)
        image = fabio.open(image_path).data
        if image.shape != strong_image.shape:
            print(f"Excluding {file}: size differs from the first image.")
            continue
        image = image.astype(np.int32)
        image[image == 4294967295] = -1
        strong_image += image
        strong_image[strong_image < 0] = -1

    return strong_image


def save_strong_image(folder_path, start_image_name, end_image_name, strong_image):
    """Save the summed strong image."""
    output_folder = os.path.join(folder_path, "summed_files")
    os.makedirs(output_folder, exist_ok=True)
    start_image_name = start_image_name.split(".")[0]
    end_image_name = end_image_name.split(".")[0]
    output_file_name = f"strong_image_{start_image_name}_{end_image_name}.tif"
    output_file_path = os.path.join(output_folder, output_file_name)
    fabio.pilatusimage.pilatusimage(data=strong_image, header=fabio.open(os.path.join(folder_path, start_image_name + ".tif")).getheader()).write(output_file_path)


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
            print(f"Strong image saved to {os.path.join(folder_path, 'summed_files')}")
    except Exception as e:
        raise e  


def create_strong_image_from_frames(file_path, start_frame=1, end_frame=None):
    """Sum frames in a file from start_frame to end_frame."""
    try:
        with fabio.open(file_path) as fabio_img:
            sum_image = None
            current_frame = 1
            print("h5 file has ", fabio_img.nframes, " frames available")
            if end_frame and end_frame > fabio_img.nframes:
                raise ValueError("The ending frame must be less than or equal to the number of frames in the file.")
            
            output_folder = os.path.join(os.path.dirname(file_path), "summed_files")
            while True:
                if start_frame <= current_frame <= (end_frame or fabio_img.nframes):
                    if sum_image is None:
                        sum_image = fabio_img.data.astype(np.int32)
                        sum_image[sum_image == 4294967295] = -1
                    
                    else:
                        to_add = fabio_img.data.astype(np.int32)
                        to_add[to_add == 4294967295] = -1
                        sum_image += to_add

                if current_frame == fabio_img.nframes or current_frame == end_frame:
                    sum_image[sum_image < 0] = -1
                    break
                fabio_img = fabio_img.next()
                current_frame += 1
            if sum_image is None:
                raise ValueError(
                    "No frames were summed; check the start/end frame indices."
                )
        output_file = os.path.join(
            output_folder,
            os.path.splitext(os.path.basename(file_path))[0]
            + f"_strong_{start_frame}-{end_frame}.tif",
        )
        os.makedirs(output_folder, exist_ok=True)
        fabio.pilatusimage.pilatusimage(data=sum_image, header=fabio_img.getheader()).write(output_file)
        print(f"Strong image saved to {output_file}")

    except Exception as e:
        raise e



def main_cli():
    parser = argparse.ArgumentParser(description="Image Processing Tool")
    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Sub-command for summing images
    sum_images_parser = subparsers.add_parser(
        "sum-images", help="Sum entire images within a folder (TIFF or H5)"
    )
    sum_images_parser.add_argument(
        "image_path_start", help="Full path to the start image."
    )
    sum_images_parser.add_argument(
        "image_path_end", help="Full path to the end image."
    )
    sum_images_parser.add_argument(
        "--exclude", nargs="*", help="List of strings to exclude from the summed image."
    )
    sum_images_parser.add_argument(
        "--headless", action="store_true", help="Run the script without the GUI."
    )

    # Sub-command for summing frames within a single file
    sum_frames_parser = subparsers.add_parser(
        "sum-frames", help="Sum frames within a single file (H5)"
    )
    sum_frames_parser.add_argument(
        "file", help="Full path to the file to sum frames from."
    )
    sum_frames_parser.add_argument(
        "start_frame", type=int, default=1, help="Starting frame to sum."
    )
    sum_frames_parser.add_argument(
        "end_frame", type=int, help="Ending frame to sum."
    )
    sum_frames_parser.add_argument(
        "--headless", action="store_true", help="Run the script without the GUI."
    )

    args = parser.parse_args()

    if args.command == "sum-images":
        folder_path_start = os.path.dirname(args.image_path_start)
        folder_path_end = os.path.dirname(args.image_path_end)
        start_image_name = os.path.basename(args.image_path_start)
        end_image_name = os.path.basename(args.image_path_end)

        if folder_path_start != folder_path_end:
            print("Start and end images must be in the same folder.")
            sys.exit(1)

        print(
            f"Summing images from {start_image_name} to {end_image_name} in {folder_path_start}"
        )
        excluded = args.exclude if args.exclude else []
        create_strong_image(
            folder_path_start, start_image_name, end_image_name, excluded
        )

    elif args.command == "sum-frames":
        file_path = args.file
        file_name = os.path.basename(file_path)
        print(f"Summing frames {args.start_frame} to {args.end_frame} in {file_name}")
        create_strong_image_from_frames(
            file_path, args.start_frame, args.end_frame
        )
    else:
        parser.print_help()


def main():
    app = QApplication(sys.argv)

    if "--headless" in sys.argv or "-h" in sys.argv or "--help" in sys.argv:
        main_cli()
    else:
        if 'sum-images' in sys.argv: 
            main_gui = SumImagesGUI()
            main_gui.show()
        elif 'sum-frames' in sys.argv:
            main_gui = SumFramesGUI()
            main_gui.show()
        else:
            sys.exit(1)
        app.exec_()


if __name__ == "__main__":
    main()
