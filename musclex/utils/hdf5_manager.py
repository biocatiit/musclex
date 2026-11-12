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

# File from BioXTAS RAW named originally SASFileIO.py but modified to serve the purpose of this software

import os
import re
import traceback
from contextlib import ExitStack
import hdf5plugin
import fabio
import h5py
import numpy as np

############################
# --- ## Load image files: ##
############################


def loadFabio(filename, hdf5_file=None, next_image=None):
    if hdf5_file is None:
        fabio_img = fabio.open(filename)
    else:
        fabio_img = hdf5_file

    num_frames = fabio_img.nframes

    if num_frames == 1:
        data = fabio_img.data
        hdr = fabio_img.getheader()

        img = [data]
        img_hdr = [hdr]

    else:
        if next_image is None:
            img = [None for i in range(fabio_img.nframes)]
            img_hdr = [None for i in range(fabio_img.nframes)]

            img[0] = fabio_img.data
            img_hdr[0] = fabio_img.getheader()

            for i in range(1, fabio_img.nframes):
                fabio_img = fabio_img.next()
                img[i] = fabio_img.data
                img_hdr[i] = fabio_img.getheader()

        else:
            frame = fabio_img.get_frame(next_image)
            data = frame.data
            hdr = frame.header

            img = [data]
            img_hdr = [hdr]

    if next_image is None or next_image == fabio_img.nframes - 1:
        fabio_img.close()

    return img, img_hdr, num_frames


##########################################
# --- ## Parse Counter Files and Headers ##
##########################################


def parseCHESSF2CTSfile(filename, new_filename=None):

    timeMonitorPattern = re.compile("\d*-second\s[a-z]*\s[()A-Z,]*\s\d*\s\d*")
    closedShutterCountPattern = re.compile("closed\s\d*")
    datePattern = re.compile("#D\s.*\n")

    with open(filename[:-3] + "cts", "rU") as f:

        mon1, mon2, exposure_time, closed_shutter_count = None, None, None, None

        for line in f:
            timeMonitor_match = timeMonitorPattern.search(line)
            closedShutterCount_match = closedShutterCountPattern.search(line)
            date_match = datePattern.search(line)

            if timeMonitor_match:
                exposure_time = int(timeMonitor_match.group().split("-")[0])
                mon1 = int(timeMonitor_match.group().split(" ")[3])
                mon2 = int(timeMonitor_match.group().split(" ")[4])

            if closedShutterCount_match:
                closed_shutter_count = int(
                    closedShutterCount_match.group().split(" ")[1]
                )

            if date_match:
                try:
                    date = date_match.group()[3:-1]
                except Exception:
                    date = "Error loading date"

    background = closed_shutter_count * exposure_time

    counters = {
        "closedShutterCnt": closed_shutter_count,
        "mon1": mon1,
        "mon2": mon2,
        "bgcount": background,
        "exposureTime": exposure_time,
        "date": date,
    }

    return counters


def parseCHESSEIGER4MCountFile(filename, new_filename=None):
    """Loads information from the counter file at CHESS, id7a from
    the image filename. EIGER .h5 files with 1-based frame numbers"""
    dir, file = os.path.split(filename)
    underscores = file.split("_")

    countFile = underscores[0]

    filenumber = int(underscores[-3])

    try:
        frame_number = int(underscores[-1].split(".")[0])
    except Exception:
        frame_number = 0

    # REG: if user root file name contains underscores, include those
    # note: must start at -3 to leave out "data" in image name

    if len(underscores) > 3:
        for each in underscores[1:-3]:
            countFile += "_" + each

    countFilename = os.path.join(dir, countFile)

    with open(countFilename, "rU") as f:
        allLines = f.readlines()

    line_num = 0
    start_found = False
    start_idx = None
    label_idx = None
    date_idx = None

    for eachLine in allLines:
        splitline = eachLine.split()

        if len(splitline) > 1:
            if splitline[0] == "#S" and splitline[1] == str(filenumber):
                start_found = True
                start_idx = line_num

            if splitline[0] == "#D" and start_found:
                date_idx = line_num

            if splitline[0] == "#L" and start_found:
                label_idx = line_num
                break

        line_num = line_num + 1

    counters = {}
    try:
        if start_idx and label_idx:
            labels = allLines[label_idx].split()
            # REG: hdf5 indices start at 1 not 0 as was our Pilatus convention!
            vals = allLines[label_idx + 0 + frame_number].split()

        for idx in range(0, len(vals)):
            counters[labels[idx + 1]] = vals[idx]

        if date_idx:
            counters["date"] = allLines[date_idx][3:-1]

    except:
        print("Error loading CHESS id7a counter file")

    return counters


def parseCHESSG1CountFile(filename, new_filename=None):
    """Loads information from the counter file at CHESS, G1 from
    the image filename"""
    dir, file = os.path.split(filename)
    underscores = file.split("_")

    countFile = underscores[0]

    filenumber = int(underscores[-2].strip("scan"))

    try:
        frame_number = int(underscores[-1].split(".")[0])
    except Exception:
        frame_number = 0

    if len(underscores) > 3:
        for each in underscores[1:-2]:
            countFile += "_" + each

    countFilename = os.path.join(dir, countFile)

    with open(countFilename, "rU") as f:
        allLines = f.readlines()

    line_num = 0
    start_found = False
    start_idx = None
    label_idx = None
    date_idx = None

    for eachLine in allLines:
        splitline = eachLine.split()

        if len(splitline) > 1:
            if splitline[0] == "#S" and splitline[1] == str(filenumber):
                start_found = True
                start_idx = line_num

            if splitline[0] == "#D" and start_found:
                date_idx = line_num

            if splitline[0] == "#L" and start_found:
                label_idx = line_num
                break

        line_num = line_num + 1

    counters = {}
    try:
        if start_idx and label_idx:
            labels = allLines[label_idx].split()
            vals = allLines[label_idx + 1 + frame_number].split()

        for idx in range(0, len(vals)):
            counters[labels[idx + 1]] = vals[idx]

        if date_idx:
            counters["date"] = allLines[date_idx][3:-1]

    except Exception:
        print("Error loading G1 header")

    return counters


def parseCHESSG1CountFileWAXS(filename, new_filename=None):
    """Loads information from the counter file at CHESS, G1 from
    the image filename"""

    dir, file = os.path.split(filename)
    underscores = file.split("_")

    countFile = underscores[0]

    filenumber = int(underscores[-2].strip("scan"))

    try:
        frame_number = int(underscores[-1].split(".")[0])
    except Exception:
        frame_number = 0

    if len(underscores) > 3:
        for each in underscores[1:-3]:
            countFile += "_" + each

    countFilename = os.path.join(dir, countFile)

    with open(countFilename, "rU") as f:
        allLines = f.readlines()

    line_num = 0
    start_found = False
    start_idx = None
    label_idx = None
    date_idx = None

    for eachLine in allLines:
        splitline = eachLine.split()

        if len(splitline) > 1:
            if splitline[0] == "#S" and splitline[1] == str(filenumber):
                start_found = True
                start_idx = line_num

            if splitline[0] == "#D" and start_found:
                date_idx = line_num

            if splitline[0] == "#L" and start_found:
                label_idx = line_num
                break

        line_num = line_num + 1

    counters = {}
    try:
        if start_idx and label_idx:
            labels = allLines[label_idx].split()
            vals = allLines[label_idx + 1 + frame_number].split()

        for idx in range(0, len(vals)):
            counters[labels[idx + 1]] = vals[idx]

        if date_idx:
            counters["date"] = allLines[date_idx][3:-1]

    except Exception:
        print("Error loading G1 header")

    return counters


def parseCHESSG1CountFileEiger(filename, new_filename=None):
    """Loads information from the counter file at CHESS, G1 from
    the image filename"""

    dirname, file = os.path.split(filename)

    dirname = os.path.dirname(dirname)
    underscores = file.split("_")

    countFile = underscores[0]

    filenumber = int(underscores[-3].strip("scan"))

    try:
        frame_number = int(underscores[-1].split(".")[0]) - 1
    except Exception:
        frame_number = 0

    if len(underscores) > 3:
        for each in underscores[1:-3]:
            countFile += "_" + each

    countFilename = os.path.join(dirname, countFile)

    with open(countFilename, "rU") as f:
        allLines = f.readlines()

    line_num = 0
    start_found = False
    start_idx = None
    label_idx = None
    date_idx = None

    for eachLine in allLines:
        splitline = eachLine.split()

        if len(splitline) > 1:
            if splitline[0] == "#S" and splitline[1] == str(filenumber):
                start_found = True
                start_idx = line_num

            if splitline[0] == "#D" and start_found:
                date_idx = line_num

            if splitline[0] == "#L" and start_found:
                label_idx = line_num
                break

        line_num = line_num + 1

    counters = {}

    try:
        if start_idx and label_idx:
            labels = allLines[label_idx].split()
            vals = allLines[label_idx + 1 + frame_number].split()

        for idx in range(0, len(vals)):
            counters[labels[idx + 1]] = vals[idx]

        if date_idx:
            counters["date"] = allLines[date_idx][3:-1]

    except Exception:
        print("Error loading G1 header")

    return counters


def parseMAXLABI911HeaderFile(filename, new_filename=None):

    filepath, ext = os.path.splitext(filename)
    hdr_file = filename + ".hdr"

    with open(hdr_file, "rU") as f:
        all_lines = f.readlines()

    counters = {}

    for each_line in all_lines:
        split_lines = each_line.split("=")
        key = split_lines[0]
        counters[key] = split_lines[-1][:-1]

    return counters


def parseMAXLABI77HeaderFile(filename, new_filename=None):

    filepath, ext = os.path.splitext(filename)
    hdr_file = filename + ".hdr"

    with open(hdr_file, "rU") as f:
        all_lines = f.readlines()

    counters = {}

    for each_line in all_lines:

        split_lines = each_line.split()
        key = split_lines[0]

        if key == "Start:":
            counters["date"] = " ".join(split_lines[1:6])
            counters["end_time"] = split_lines[-1]
        elif key == "Sample:":
            counters["sample"] = split_lines[1]
            counters["code"] = split_lines[-1]
        elif key == "MAXII":
            counters["current_begin"] = split_lines[4]
            counters["current_end"] = split_lines[6]
            counters["current_mean"] = split_lines[-1]
        elif key == "SampleTemperature:":
            counters["temp_begin"] = split_lines[2]
            counters["temp_end"] = split_lines[2]
            counters["temp_mean"] = split_lines[6]
        elif key == "SampleDiode:":
            counters["SmpDiode_begin"] = split_lines[2]
            counters["SmpDiode_end"] = split_lines[2]
            counters["SmpDiode_mean"] = split_lines[6]
        elif key == "BeamstopDiode:":
            counters["BmStpDiode_avg"] = split_lines[2]
        elif key == "IonChamber:":
            counters["IonDiode_avg"] = split_lines[2]
        elif key == "Tube":
            counters["vacuum"] = split_lines[-1]
        elif key == "ExposureTime:":
            counters["exposureTime"] = split_lines[1]
        elif key == "MarCCD":
            counters["diameter"] = split_lines[4]
            counters["binning"] = split_lines[-1]
        elif key == "BeamCenterX:":
            counters["xCenter"] = split_lines[1]
            counters["yCenter"] = split_lines[3]

    return counters


def parseBioCATlogfile(filename, new_filename=None):
    datadir, fname = os.path.split(filename)

    if new_filename is not None:
        # BioCAT Eiger

        sname_val = int(new_filename.split(".")[0].split("_")[-1])
        searchName = ".".join(fname.split(".")[:-1])

        if fname.endswith("master.h5"):
            countFilename = os.path.join(
                datadir, "_".join(fname.split("_")[:-1]) + ".log"
            )
            searchName = "_".join(searchName.split("_")[:-1]) + "_{:06d}".format(
                sname_val
            )
        else:
            countFilename = os.path.join(
                datadir, "_".join(fname.split("_")[:-2]) + ".log"
            )
            searchName = "_".join(searchName.split("_")[:-2]) + "_{:06d}".format(
                sname_val
            )

    else:
        # BioCAT Pilatus
        countFilename = os.path.join(datadir, "_".join(fname.split("_")[:-1]) + ".log")
        searchName = ".".join(fname.split(".")[:-1])

    with open(countFilename, "rU") as f:
        allLines = f.readlines()

    line_num = 0

    counters = {}

    offset = 0

    for i, line in enumerate(allLines):
        if line.startswith("#"):
            if line.startswith("#Filename") or line.startswith("#image"):
                labels = line.strip("#").split("\t")
                offset = i
            else:
                key = line.strip("#").split(":")[0].strip()
                val = ":".join(line.strip("#").split(":")[1:])
                if key in counters:
                    counters[key] = counters[key] + "\n" + val.strip()
                else:
                    counters[key] = val.strip()
        else:
            break

    test_idx = int(searchName.split("_")[-1]) + offset

    if test_idx < len(allLines) and searchName in allLines[test_idx]:
        line_num = test_idx
    else:
        for a in range(1, len(allLines)):
            if searchName in allLines[a]:
                line_num = a

    if line_num > 0:
        vals = allLines[line_num].split("\t")

        for a in range(len(labels)):
            counters[labels[a].strip()] = vals[a].strip()

    else:
        counters = {}

    return counters


def parseBL19U2HeaderFile(filename, new_filename=None):
    fname, ext = os.path.splitext(filename)

    countFilename = fname + ".txt"

    counters = {}

    with open(countFilename, "rU") as f:
        for line in f:
            name = line.split(":")[0]
            value = ":".join(line.split(":")[1:])
            counters[name.strip()] = value.strip()

    return counters


def parsePetraIIIP12EigerFile(filename, new_filename=None):
    if new_filename:
        fnum = int(new_filename.split("_")[-1].split(".")[0])
    else:
        fnum = 1

    data_path, data_name = os.path.split(filename)

    header_name = "_".join(data_name.split("_")[:2]) + "_%05i.txt" % (fnum)

    header_path = os.path.join(os.path.split(data_path)[0], "header")

    countFilename = os.path.join(header_path, header_name)

    counters = {}

    with open(countFilename, "rU") as f:
        for line in f:
            name = line.split(":")[0]
            value = ":".join(line.split(":")[1:])
            counters[name.strip()] = value.strip()

    return counters


#################################################################
# --- ** Header and Image formats **
#################################################################
# To add new header types, write a parse function and append the
# dictionary header_types below
#################################################################

all_header_types = {
    "None": None,
    "F2, CHESS": parseCHESSF2CTSfile,
    "G1, CHESS": parseCHESSG1CountFile,
    "CHESS EIGER 4M": parseCHESSEIGER4MCountFile,
    "G1 WAXS, CHESS": parseCHESSG1CountFileWAXS,
    "G1 Eiger, CHESS": parseCHESSG1CountFileEiger,
    "I711, MaxLab": parseMAXLABI77HeaderFile,
    "I911-4 Maxlab": parseMAXLABI911HeaderFile,
    "BioCAT, APS": parseBioCATlogfile,
    "BL19U2, SSRF": parseBL19U2HeaderFile,
    "P12 Eiger, Petra III": parsePetraIIIP12EigerFile,
}

all_image_types = {
    "Pilatus": loadFabio,
    "CBF": loadFabio,
    "ADSC Quantum": loadFabio,
    "Bruker": loadFabio,
    "Gatan Digital Micrograph": loadFabio,
    "EDNA-XML": loadFabio,
    "ESRF EDF": loadFabio,
    "Nonius KappaCCD": loadFabio,
    "Fit2D spreadsheet": loadFabio,
    "General Electric": loadFabio,
    "Hamamatsu CCD": loadFabio,
    "HDF5 (Hierarchical data format)": loadFabio,
    "MarCCD 165": loadFabio,
    "Mar345": loadFabio,
    "Numpy 2D Array": loadFabio,
    "Oxford Diffraction": loadFabio,
    "Pixi": loadFabio,
    "Portable aNy Map": loadFabio,
    "Rigaku SAXS format": loadFabio,
    "16 bit TIF": loadFabio,
    "MPA (multiwire)": loadFabio,
    "Eiger": loadFabio,
    "Rigaku HiPix": loadFabio,
    # 'NeXus'           : loadNeXusFile,
}


def loadAllHeaders(filename, image_type, header_type):
    """returns the image header and the info from the header file only."""

    try:
        file_type = checkFileType(filename)
        # print file_type
    except IOError:
        raise
    except Exception as msg:
        print(str(msg))
        file_type = None

    if file_type == "hdf5":
        try:
            hdf5_file = fabio.open(filename)
            file_type = "image"
        except Exception:
            pass
    else:
        hdf5_file = None

    if hdf5_file is not None:
        img, imghdr, num_frames = loadImage(filename, hdf5_file, next_image=0)
    else:
        img, imghdr, num_frames = loadImage(filename, next_image=0)

    if len(img) > 1 or num_frames > 1 or hdf5_file is not None:
        temp_filename = os.path.split(filename)[1].split(".")

        if len(temp_filename) > 1:
            temp_filename[-2] = temp_filename[-2] + "_%05i" % (1)
        else:
            temp_filename[0] = temp_filename[0] + "_%05i" % (1)

        new_filename = ".".join(temp_filename)
    else:
        new_filename = os.path.split(filename)[1]

    if header_type != "None":
        hdr = loadHeader(filename, new_filename, header_type)
    else:
        hdr = None

    masks = {
        "BeamStopMask": [None, None, None],
        "TransparentBSMask": [None, None, None],
    }
    tbs_mask = masks["TransparentBSMask"][0]

    if tbs_mask is not None:
        if isinstance(img, list):
            roi_counter = img[0][
                tbs_mask == 1
            ].sum()  # In the case of multiple images in the same file, load the ROI for the first one
        else:
            roi_counter = img[tbs_mask == 1].sum()

        if hdr is None:
            hdr = {"roi_counter": roi_counter}
        else:
            hdr["roi_counter"] = roi_counter

    return imghdr, hdr


def loadHeader(filename, new_filename, header_type):
    """returns header information based on the *image* filename
    and the type of headerfile"""

    if header_type != "None":
        try:
            if new_filename != os.path.split(filename)[1]:
                hdr = all_header_types[header_type](filename, new_filename)
            else:
                hdr = all_header_types[header_type](filename)
        except IOError as io:
            print(str(io).replace("u'", ""))
            # traceback.print_exc()
        except Exception as e:
            # print(e)
            # traceback.print_exc()
            print(
                "Header file for : "
                + str(filename)
                + " could not be read or contains incorrectly formatted data. "
            )
    else:
        hdr = {}

    # Clean up headers by removing spaces in header names and non-unicode characters)
    if hdr is not None:
        hdr = {
            (
                key.replace(" ", "_").translate(str.maketrans("", "", "()[]"))
                if isinstance(key, str)
                else key
            ): hdr[key]
            for key in hdr
        }
        # hdr = { key : str(hdr[key], errors='ignore') if isinstance(hdr[key], str)
        #     else hdr[key] for key in hdr}

    return hdr


def loadImage(filename, hdf5_file=None, next_image=None):
    """returns the loaded image based on the image filename
    and image type."""
    image_type = "Pilatus"

    num_frames = 1

    try:
        if all_image_types[image_type] == loadFabio:
            img, imghdr, num_frames = all_image_types[image_type](
                filename, hdf5_file, next_image
            )
        else:
            img, imghdr = all_image_types[image_type](filename)
    except (
        ValueError,
        TypeError,
        KeyError,
        fabio.fabioutils.NotGoodReader,
        Exception,
    ) as msg:
        # traceback.print_exc()
        print("Error loading image, " + str(msg))

    if not isinstance(img, list):
        img = [img]
    if not isinstance(imghdr, list):
        imghdr = [imghdr]

    # Clean up headers by removing spaces in header names and non-unicode characters)
    for hdr in imghdr:
        if hdr is not None:
            hdr = {
                (
                    key.replace(" ", "_").translate(str.maketrans("", "", "()[]"))
                    if isinstance(key, str)
                    else key
                ): hdr[key]
                for key in hdr
            }
            # hdr = { key : str(hdr[key], errors='ignore') if isinstance(hdr[key], str)
            #     else hdr[key] for key in hdr}

    return img, imghdr, num_frames


#################################
# --- ** MAIN LOADING FUNCTION **
#################################

def _matching_master_data_file(master_path, external_filename):
    """Find the matching sibling data file for a master with a broken link."""
    master_name = os.path.basename(master_path)
    master_match = re.match(
        r"^(.*)_master\.(?:h5|hdf5|nxs)$", master_name, re.IGNORECASE
    )
    data_match = re.search(
        r"(_data_[^/\\]+\.(?:h5|hdf5))$",
        os.path.basename(external_filename),
        re.IGNORECASE,
    )
    if master_match is None or data_match is None:
        return None

    expected_name = master_match.group(1) + data_match.group(1)
    directory = os.path.dirname(os.path.abspath(master_path))
    matches = [
        name
        for name in os.listdir(directory)
        if name.lower() == expected_name.lower()
    ]
    if len(matches) != 1:
        return None
    candidate = os.path.join(directory, matches[0])
    return candidate if os.path.isfile(candidate) else None


def _hdf5_dataset_frames(path, dataset_path=None):
    """Return loader specs for the most likely 2-D detector dataset in *path*.

    AreaDetector files used by Diamond commonly store data as
    ``scan dimensions × height × width``. Flatten those leading dimensions
    explicitly. When ``dataset_path`` is supplied, use that dataset instead
    of applying the detector-dataset heuristic.
    """
    candidates = []
    try:
        with h5py.File(path, "r") as h5_file:
            if dataset_path is not None:
                dataset = h5_file.get(dataset_path)
                if not isinstance(dataset, h5py.Dataset):
                    return [], None
                if dataset.dtype.kind not in "iuf":
                    return [], None
                shape = tuple(int(value) for value in dataset.shape)
                if len(shape) < 2 or min(shape[-2:]) < 1:
                    return [], None
                frame_count = int(np.prod(shape[:-2])) if len(shape) > 2 else 1
                specs = [
                    (
                        "h5_dataset",
                        os.path.abspath(dataset.file.filename),
                        dataset.name.lstrip("/"),
                        frame_index,
                    )
                    for frame_index in range(frame_count)
                ]
                return specs, shape[-2:]

            def collect(name, obj):
                if not isinstance(obj, h5py.Dataset) or obj.dtype.kind not in "iuf":
                    return
                shape = tuple(int(value) for value in obj.shape)
                if len(shape) < 2 or min(shape[-2:]) < 64:
                    return
                # Eiger master files can keep calibration arrays locally while
                # exposing detector frames through external links. visititems()
                # does not follow those links, so exclude calibration datasets.
                path_parts = {
                    part.lower().replace("-", "_") for part in name.split("/")
                }
                auxiliary_parts = {
                    "flatfield",
                    "flat_field",
                    "pixel_mask",
                    "mask",
                    "bad_pixel_mask",
                    "gain",
                    "gain_map",
                    "validity",
                    "quality",
                }
                if path_parts & auxiliary_parts:
                    return
                frame_count = int(np.prod(shape[:-2])) if len(shape) > 2 else 1
                data_name = os.path.basename(name).lower()
                score = (
                    data_name in ("data", "image", "images", "detector_data"),
                    frame_count,
                    shape[-2] * shape[-1],
                )
                candidates.append(
                    (
                        score,
                        os.path.abspath(obj.file.filename),
                        (obj.name or name).lstrip("/"),
                        shape,
                        frame_count,
                    )
                )

            visited_groups = set()

            def walk(group):
                group_key = (
                    os.path.normcase(os.path.realpath(group.file.filename)),
                    group.name,
                )
                if group_key in visited_groups:
                    return
                visited_groups.add(group_key)
                for name in group:
                    link = group.get(name, getlink=True)
                    try:
                        obj = group.get(name)
                    except (KeyError, OSError, RuntimeError, ValueError):
                        obj = None
                    if obj is None and isinstance(link, h5py.ExternalLink):
                        fallback_path = _matching_master_data_file(
                            h5_file.filename, link.filename
                        )
                        if fallback_path is None:
                            continue
                        try:
                            with h5py.File(fallback_path, "r") as fallback_file:
                                fallback_obj = fallback_file.get(link.path.lstrip("/"))
                                if isinstance(fallback_obj, h5py.Group):
                                    walk(fallback_obj)
                                elif isinstance(fallback_obj, h5py.Dataset):
                                    collect(fallback_obj.name, fallback_obj)
                        except (KeyError, OSError, RuntimeError, ValueError):
                            continue
                        continue
                    if isinstance(obj, h5py.Group):
                        walk(obj)
                    elif isinstance(obj, h5py.Dataset):
                        collect(obj.name or name, obj)

            walk(h5_file)
    except (KeyError, OSError, RuntimeError, ValueError):
        return [], None

    if not candidates:
        return [], None
    _score, source_path, dataset_path, shape, frame_count = max(
        candidates, key=lambda item: item[0]
    )
    specs = [
        ("h5_dataset", source_path, dataset_path, frame_index)
        for frame_index in range(frame_count)
    ]
    return specs, shape[-2:]


def loadFile(filename, no_processing=False, return_all_images=True):
    """Loads a file an returns a SAS Measurement Object (SASM) and the full image if the
    selected file was an Image file

     NB: This is the function used to load any type of file in RAW
    """
    try:
        file_type = checkFileType(filename)
        # print file_type
    except IOError:
        raise
    except Exception as msg:
        print(str(msg))
        file_type = None

    if file_type == "hdf5":
        try:
            # print("FILENAME: " + str(filename)) #NICKA DEBUG
            hdf5_file = fabio.open(filename)
            file_type = "image"
        except Exception as e:
            try:
                generic_images = _load_generic_hdf5(filename)
            except Exception as fallback_error:
                print("Error loading HDF5 dataset. Error:", fallback_error)
                return None, None
            if generic_images is not None:
                return generic_images
            print("Error loading HDF5 file.  Error:", end=" ")
            print(e)
            return None, None
    else:
        hdf5_file = None

    if file_type == "image":
        try:
            sasm, img = loadImageFile(filename, hdf5_file, return_all_images)
        except (ValueError, AttributeError) as msg:
            print("No data could be retrieved from the file, unknown format.")
            if hdf5_file is not None:
                try:
                    generic_images = _load_generic_hdf5(filename)
                except Exception as fallback_error:
                    print("Error loading HDF5 dataset. Error:", fallback_error)
                    return None, None
                if generic_images is not None:
                    return generic_images
            traceback.print_exc()
            return None, None
        # except Exception:
        # traceback.print_exc()
        # raise

        # Always do some post processing for image files
        if not isinstance(sasm, list):
            sasm = [sasm]

    if not isinstance(sasm, list) and (sasm is None or len(sasm.i) == 0):
        print("No data could be retrieved from the file, unknown format.")
        return None, None

    return sasm, img


def _load_generic_hdf5(filename):
    """Load numeric image datasets when Fabio has no reader for an HDF5 layout."""
    frame_specs, _shape = _hdf5_dataset_frames(filename)
    if not frame_specs:
        return None

    loaded_images = []
    with ExitStack() as file_stack:
        opened_files = {}
        for _kind, _path, dataset_path, frame_index in frame_specs:
            source_path = _path
            file_key = os.path.normcase(os.path.realpath(source_path))
            if file_key not in opened_files:
                opened_files[file_key] = file_stack.enter_context(
                    h5py.File(source_path, "r")
                )
            dataset = opened_files[file_key][dataset_path]
            leading_shape = dataset.shape[:-2]
            if leading_shape:
                leading_index = np.unravel_index(frame_index, leading_shape)
                image = dataset[leading_index]
            else:
                image = dataset[()]
            loaded_images.append(np.asarray(image))

    base, ext = os.path.splitext(os.path.basename(filename))
    image_names = [
        f"{base}_{frame_index + 1:05d}{ext}"
        for frame_index in range(len(loaded_images))
    ]
    return image_names, loaded_images


def loadImageFile(filename, hdf5_file=None, return_all_images=True):
    hdr_fmt = "None"

    if hdf5_file is not None:
        is_hdf5 = True
    else:
        is_hdf5 = False

    load_one_frame = False

    if is_hdf5 and hdf5_file.nframes > 1:
        num_frames = hdf5_file.nframes
        load_one_frame = True
    else:
        num_frames = 1

    loaded_data = []
    sasm_list = []

    for file_num in range(num_frames):
        if load_one_frame:
            new_data, new_hdr, _ = loadImage(filename, hdf5_file, file_num)

        else:
            new_data, new_hdr, _ = loadImage(filename, hdf5_file)

        if return_all_images:
            loaded_data.extend(new_data)
        elif not return_all_images and file_num == 0:
            loaded_data.append(new_data[0])

        offset = 0

        # Process all loaded images into sasms
        for i in range(len(new_data)):
            img = new_data[i]
            img_hdr = new_hdr[i]

            if i == 0 and (len(new_data) > 1 or is_hdf5):

                temp_filename = os.path.split(filename)[1].split(".")

                if len(temp_filename) > 1:
                    temp_filename[-2] = temp_filename[-2] + "_%05i" % (i + 1)
                else:
                    temp_filename[0] = temp_filename[0] + "_%05i" % (i + 1)

                new_filename = ".".join(temp_filename)

                base_hdr = hdrfile_info = loadHeader(filename, new_filename, hdr_fmt)

                if not filename.endswith("master.h5"):
                    sname_offset = int(os.path.splitext(filename)[0].split("_")[-1]) - 1
                else:
                    sname_offset = 0

                if "Number_of_images_per_file" in base_hdr:
                    mult = int(base_hdr["Number_of_images_per_file"])
                else:
                    mult = len(new_data)

                offset = sname_offset * mult

            if len(new_data) > 1 or is_hdf5:
                temp_filename = os.path.split(filename)[1].split(".")

                if len(temp_filename) > 1:
                    temp_filename[-2] = temp_filename[-2] + "_%05i" % (
                        i + file_num + offset + 1
                    )
                else:
                    temp_filename[0] = temp_filename[0] + "_%05i" % (
                        i + file_num + offset + 1
                    )

                new_filename = ".".join(temp_filename)
            else:
                new_filename = os.path.split(filename)[1]

            hdrfile_info = loadHeader(filename, new_filename, hdr_fmt)

            parameters = {
                "imageHeader": img_hdr,
                "counters": hdrfile_info,
                "filename": new_filename,
                "load_path": filename,
            }

            sasm = parameters["filename"]

            sasm_list.append(sasm)

    return sasm_list, loaded_data


def checkFileType(filename):
    """Tries to find out what file type it is and reports it back"""

    path, ext = os.path.splitext(filename)

    if ext == ".fit":
        return "fit"
    elif ext == ".fir":
        return "fir"
    elif ext == ".abs":
        return "abs"
    elif ext == ".out":
        return "out"
    elif ext == ".edf":
        return "image"
    elif ext == ".ccdraw":
        return "image"
    elif ext == ".int":
        return "int"
    elif (
        ext == ".img"
        or ext == ".imx_0"
        or ext == ".dkx_0"
        or ext == ".dkx_1"
        or ext == ".png"
        or ext == ".mpa"
    ):
        return "image"
    elif ext == ".dat" or ext == ".sub" or ext == ".txt":
        return "primus"
    elif (
        ext == ".mar1200" or ext == ".mar2400" or ext == ".mar2300" or ext == ".mar3600"
    ):
        return "image"
    elif (
        ext == ".img"
        or ext == ".sfrm"
        or ext == ".dm3"
        or ext == ".edf"
        or ext == ".xml"
        or ext == ".cbf"
        or ext == ".kccd"
        or ext == ".msk"
        or ext == ".spr"
        or ext == ".tif"
        or ext == ".mccd"
        or ext == ".mar3450"
        or ext == ".npy"
        or ext == ".pnm"
        or ext == ".No"
    ):
        return "image"
    elif ext == ".ift":
        return "ift"
    elif ext == ".csv":
        return "txt"
    elif ext.lower() in (".h5", ".hdf5", ".nxs"):
        return "hdf5"
    else:
        try:
            f = fabio.open(filename)
            return "image"
        except Exception:
            try:
                float(ext.strip("."))
            except Exception:
                return "txt"
            return "csv"
