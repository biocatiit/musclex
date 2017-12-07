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


from os import makedirs
import csv
from ..utils.file_manager import *

class DC_CSVManager():
    """
    A class taking care of writing summary file
    """
    def __init__(self, dir_path, nFrames, fix_ranges = []):
        self.nFrames = nFrames # number of frames that average

        sum_path = fullPath(dir_path, "dc_summary")

        if not exists(sum_path):
            makedirs(sum_path)

        if self.nFrames > 0:
            # if number of frames that average is specified, summary fill will be summary_nf.csv while n is number of frames
            self.filename = fullPath(sum_path, "summary_"+str(nFrames)+"f")
        else:
            # otherwise, summary file will be summary_m.csv
            self.filename = fullPath(sum_path, "summary_m")

        fix_range_names = [fr[0] for fr in fix_ranges]
        if len(fix_range_names) > 0:
            for n in fix_range_names:
                self.filename += ("_" + str(n))
        self.filename += ".csv"

        self.allpeaks_data = {} # Collect meridian peaks data
        self.offmer_data = {} # Collect off-meridian peaks data
        self.grp_to_key = {} # Map group number with key
        self.gen_data = {} # general data of Diffraction Centroid. Now, there's only integrated width
        self.headers = [] # header of csv file
        self.keyToImages = {} # Map key to image names
        self.loadSummary() # load exist summary file before adding new data

    def getRow(self, key):
        # return index of key in header
        return self.headers.index(key)

    def loadSummary(self):
        """
        Load exist summary file, and encapsulate them
        """
        self.allpeaks_data = {}
        self.offmer_data = {}
        self.gen_data = {}
        self.keyToImages = {}
        self.grp_to_key = {}
        currentGroup = -1
        header = []
        # if self.nFrames == 0:
        #     # Files are manually selected
        #     if exists(self.filename):
        #         with open(self.filename, "rb") as csvfile:
        #             reader = csv.reader(csvfile)
        #             peak_data = []
        #             images = []
        #             g_data = {}
        #             off_mer = {}
        #
        #             for row in reader:
        #                 if len(row) == 0 :
        #                     continue
        #
        #                 if row[0] == "group":
        #                     if len(images) > 0:
        #                         k = "...".join([images[0], images[-1]])
        #                         self.allpeaks_data[k] = peak_data
        #                         self.offmer_data[k] = off_mer
        #                         self.gen_data[k] = g_data
        #                         self.keyToImages[k] = images
        #                         self.grp_to_key[int(currentGroup)] = k
        #
        #                     header = copy.copy(row)
        #                     peak_data = []
        #                     off_mer = {}
        #                     images = []
        #                     g_data = {}
        #                     continue
        #
        #                 elif row[0] != "":
        #                     currentGroup = row[0]
        #                     images.append(row[1])
        #                     g_data["integration area width"] = row[2]
        #                     for i in range(3, len(row) - 3):
        #                         if "centroid" in header[i]:
        #                             if "average" in header[i]:
        #                                 continue
        #                             head = header[i]
        #                             head = head.split(" ")
        #                             if "_" in head[0] and head[1] == "59":
        #                                 quadrant = head[0]
        #                                 off_mer[quadrant] = {
        #                                     "centroid": [row[header.index(quadrant + " 59 " +"centroid")],
        #                                                  row[header.index(quadrant + " 51 " +"centroid")]],
        #                                     "baseline": [row[header.index(quadrant + " 59 " + "baseline")],
        #                                                  row[header.index(quadrant + " 51 " + "baseline")]],
        #                                     "intensity": [row[header.index(quadrant + " 59 " + "intensity")],
        #                                                  row[header.index(quadrant + " 51 " + "intensity")]],
        #                                 }
        #                             else:
        #                                 side = head[0]
        #                                 name = head[1]
        #                                 new_peak = {
        #                                     "side" : side,
        #                                     "name" : name,
        #                                     "centroid" : row[i],
        #                                     "baseline" : row[header.index(side + " " + name + " " + "baseline")],
        #                                     "intensity" : row[header.index(side + " " + name + " " + "intensity")]
        #                                 }
        #                                 peak_data.append(new_peak)
        #                 else:
        #                     images.append(row[1])
        #
        #             if len(images) > 0:
        #                 k = "...".join([images[0], images[-1]])
        #                 self.allpeaks_data[k] = peak_data
        #                 self.offmer_data[k] = off_mer
        #                 self.gen_data[k] = g_data
        #                 self.keyToImages[k] = images
        #                 self.grp_to_key[int(currentGroup)] = k
        # else:
        # Files are selected by auto-grouping
        if exists(self.filename):
            with open(self.filename, "r") as csvfile:
                reader = csv.reader(csvfile)
                peak_data = []
                images = []
                g_data = {}
                off_mer = {}

                for row in reader:
                    if len(row) == 0:
                        continue

                    if row[0] == "group":
                        header = row[:]
                        continue

                    if len(row[0]) > 0:
                        if len(images) > 0:
                            k = "...".join([images[0], images[-1]])
                            self.allpeaks_data[k] = peak_data
                            self.offmer_data[k] = off_mer
                            self.gen_data[k] = g_data
                            self.keyToImages[k] = images
                            self.grp_to_key[int(currentGroup)] = k
                        peak_data = []
                        images = []
                        g_data = {}
                        off_mer = {}

                    if row[0] != "":
                        currentGroup = row[0]
                        images.append(row[1])
                        g_data["integration area width"] = row[2]
                        for i in range(3, len(row) - 3):
                            if "centroid" in header[i]:
                                if "average" in header[i]:
                                    continue
                                head = header[i]
                                head = head.split(" ")
                                if "_" in head[0] and head[1] == "59":
                                    quadrant = head[0]
                                    off_mer[quadrant] = {
                                        "centroid": [row[header.index(quadrant + " 59 " +"centroid")],
                                                     row[header.index(quadrant + " 51 " +"centroid")]],
                                        "baseline": [row[header.index(quadrant + " 59 " + "baseline")],
                                                     row[header.index(quadrant + " 51 " + "baseline")]],
                                        "intensity": [row[header.index(quadrant + " 59 " + "intensity")],
                                                     row[header.index(quadrant + " 51 " + "intensity")]],
                                    }
                                else:
                                    side = head[0]
                                    name = head[1]
                                    new_peak = {
                                        "side": side,
                                        "name": name,
                                        "centroid": row[i],
                                        "baseline": row[header.index(side + " " + name + " " + "baseline")],
                                        "intensity": row[header.index(side + " " + name + " " + "intensity")]
                                    }
                                    peak_data.append(new_peak)
                    else:
                        images.append(row[1])

                if len(images) > 0:
                    k = "...".join([images[0], images[-1]])
                    self.allpeaks_data[k] = peak_data
                    self.offmer_data[k] = off_mer
                    self.gen_data[k] = g_data
                    self.keyToImages[k] = images
                    self.grp_to_key[int(currentGroup)] = k

    def writeNewData(self, info):
        """
        Add new data (DiffractionCentroids info) to encapsulated member, and rewrite summary
        :param info: DiffractionCentroids info (dict)
        """
        fileList = info["filelist"]
        k = "...".join([fileList[0], fileList[-1]])
        if "summary_m" in self.filename:
            grpnum = len(self.grp_to_key.keys()) + 1
            for (group, key) in self.grp_to_key.items():
                if key == k:
                    grpnum = group
            self.grp_to_key[grpnum] = k
        else:
            self.grp_to_key[info["grp_num"]] = k
        self.keyToImages[k] = fileList

        if k in self.allpeaks_data:
            del self.allpeaks_data[k]

        if k in self.offmer_data:
            del self.offmer_data[k]

        data = []
        for side in ["top", "bottom"]:
            centroids = info[side + "_centroids"]
            peaks = info[side + "_peaks"]
            baselines = info[side + "_baselines"]
            areas = info[side + "_areas"]
            names = info[side + "_names"]
            reject_state = info["reject"][side]
            for i in range(len(peaks)):
                peak_dict = {}
                peak_dict["side"] = side
                peak_dict["name"] = names[i]
                if names[i] in reject_state:
                    peak_dict["centroid"] = "_"+str(centroids[i])
                    peak_dict["peak"] = "_"+str(peaks[i])
                    peak_dict["intensity"] = "_"+str(areas[i])
                    peak_dict["baseline"] = "_"+str(baselines[i])
                else:
                    peak_dict["centroid"] = str(centroids[i])
                    peak_dict["peak"] = str(peaks[i])
                    peak_dict["intensity"] = str(areas[i])
                    peak_dict["baseline"] = str(baselines[i])

                data.append(peak_dict)

        off_mer_data = {}
        if "off_mer_baselines" in info:
            for q in ["top_left", "top_right", "bottom_left", "bottom_right"]:
                dat = {
                    "baseline" : info["off_mer_baselines"][q],
                    "centroid" : info["off_mer_peak_info"][q]["centroids"],
                    "intensity" : info["off_mer_peak_info"][q]["areas"]
                }
                off_mer_data[q] = dat

        self.allpeaks_data[k] = data
        self.offmer_data[k] = off_mer_data
        l,r = info["int_area"]
        self.gen_data[k] = {"integration area width":abs(r-l)}
        self.rewriteSummary()

    def getOffMerHeaders(self):
        """
        Get Off-meridian header for csv file
        """
        header = []
        for t in ["centroid", "baseline", "intensity"]:
            for p in ["59", "51"]:
                for q in ["top_left", "top_right", "bottom_left", "bottom_right"]:
                    header.append(q + " " + p + " "+t)
                header.append(p + " average "+t)
        return header

    def toInt(self, s):
        try:
            result = int(s)
        except Exception:
            result = s
        return result

    def rewriteSummary(self):
        """
        Re-write summary file from encapsulated data
        :return:
        """
        # if self.nFrames == 0:
        #     # If files are selected manually
        #     with open(self.filename, "w") as csvfile:
        #         writer = csv.writer(csvfile, delimiter = ",")
        #         grpKey = sorted(self.grp_to_key.items(), key= lambda (g,k) : int(g))
        #         for (g, k) in grpKey:
        #             peak_data = self.allpeaks_data[k]
        #             g_data = self.gen_data[k]
        #             filelist = self.keyToImages[k]
        #             off_mer = self.offmer_data[k]
        #
        #             ### write header for each group ###
        #             header = ["group", "filename", "integration area width"]
        #             top_peaks = sorted([d for d in peak_data if d["side"] == "top"], key=lambda d: self.toInt(d["name"]))
        #             bottom_peaks = sorted([d for d in peak_data if d["side"] == "bottom"], key=lambda d: self.toInt(d["name"]))
        #             nTop = len(top_peaks)
        #             nBottom = len(bottom_peaks)
        #             min_nPeaks = min(nTop, nBottom)
        #
        #             for i in range(min_nPeaks):
        #                 peak_header = ["top " + top_peaks[i]["name"] + " centroid", "bottom " + bottom_peaks[i]["name"] + " centroid", "average " + bottom_peaks[i]["name"] + " centroid",
        #                                "top " + top_peaks[i]["name"] + " baseline", "bottom " + bottom_peaks[i]["name"] + " baseline", "average " + bottom_peaks[i]["name"] + " baseline",
        #                                "top " + top_peaks[i]["name"] + " intensity", "bottom " + bottom_peaks[i]["name"] + " intensity", "average " + bottom_peaks[i]["name"] + " intensity"]
        #                 header.extend(peak_header)
        #
        #             if nTop > nBottom:
        #                 extra = "top"
        #                 extra_peaks = top_peaks
        #             else:
        #                 extra = "bottom"
        #                 extra_peaks = bottom_peaks
        #
        #             for i in range(max(nTop, nBottom) - min_nPeaks):
        #                 peak_header = [extra + " " +extra_peaks[min_nPeaks+i]["name"] + " centroid",
        #                                extra + " " +extra_peaks[min_nPeaks+i]["name"] + " baseline",
        #                                extra + " " +extra_peaks[min_nPeaks+i]["name"] + " intensity"]
        #                 header.extend(peak_header)
        #
        #             if len(off_mer.keys()) > 0:
        #                 header.extend(self.getOffMerHeaders())
        #
        #             writer.writerow(header)
        #
        #             ### Write data of each group ###
        #             for i in range(len(filelist)):
        #                 if i != 0:
        #                     write_row = ["", str(filelist[i])]
        #                 else:
        #                     write_row = [str(g), str(filelist[i]), g_data["integration area width"]]
        #                     for i in range(min_nPeaks):
        #                         peak_detail = [top_peaks[i]["centroid"], bottom_peaks[i]["centroid"],
        #                                        self.average(top_peaks[i]["centroid"], bottom_peaks[i]["centroid"]),
        #                                        top_peaks[i]["baseline"], bottom_peaks[i]["baseline"],
        #                                        self.average(top_peaks[i]["baseline"], bottom_peaks[i]["baseline"]),
        #                                        top_peaks[i]["intensity"], bottom_peaks[i]["intensity"],
        #                                        self.average(top_peaks[i]["intensity"], bottom_peaks[i]["intensity"])
        #                                        ]
        #                         write_row.extend(peak_detail)
        #
        #                     for i in range(max(nTop, nBottom) - min_nPeaks):
        #                         ind = min_nPeaks + i
        #                         peak_detail = [ extra_peaks[ind]["centroid"],
        #                                         extra_peaks[ind]["baseline"],
        #                                         extra_peaks[ind]["intensity"]]
        #                         write_row.extend(peak_detail)
        #
        #                     if len(off_mer.keys()) > 0:
        #                         for t in ["centroid", "baseline", "intensity"]:
        #                             for p in [0,1]:
        #                                 sum_val = 0.
        #                                 for q in ["top_left", "top_right", "bottom_left", "bottom_right"]:
        #                                     write_row.append(off_mer[q][t][p])
        #                                     sum_val += float(off_mer[q][t][p])
        #                                 write_row.append(sum_val/4.)
        #
        #                 writer.writerow(write_row)
        #             writer.writerow([])
        #             writer.writerow([])
        # else:
        # Files are selected by auto-grouping
        with open(self.filename, "w") as csvfile:
            writer = csv.writer(csvfile, delimiter=",")
            grpKey = sorted(self.grp_to_key.items(), key=lambda gk: int(gk[0]))
            g1, k1 = grpKey[0]
            peak_data = self.allpeaks_data[k1]

            ### write header for all group ###
            header = ["group", "filename", "integration area width"]
            top_peaks = sorted([d for d in peak_data if d["side"] == "top"], key=lambda d: self.toInt(d["name"]))
            bottom_peaks = sorted([d for d in peak_data if d["side"] == "bottom"], key=lambda d: self.toInt(d["name"]))
            nTop = len(top_peaks)
            nBottom = len(bottom_peaks)
            min_nPeaks = min(nTop, nBottom)

            for i in range(min_nPeaks):
                peak_header = ["top " + top_peaks[i]["name"] + " centroid",
                               "bottom " + bottom_peaks[i]["name"] + " centroid",
                               "average " + bottom_peaks[i]["name"] + " centroid",
                               "top " + top_peaks[i]["name"] + " baseline",
                               "bottom " + bottom_peaks[i]["name"] + " baseline",
                               "average " + bottom_peaks[i]["name"] + " baseline",
                               "top " + top_peaks[i]["name"] + " intensity",
                               "bottom " + bottom_peaks[i]["name"] + " intensity",
                               "average " + bottom_peaks[i]["name"] + " intensity"]
                header.extend(peak_header)
            header.extend(self.getOffMerHeaders())

            writer.writerow(header)
            self.headers = header

            ### Write data for each group ###
            for (g, k) in grpKey:
                off_mer = self.offmer_data[k]
                peak_data = self.allpeaks_data[k]
                g_data = self.gen_data[k]
                filelist = self.keyToImages[k]
                top_peaks = sorted([d for d in peak_data if d["side"] == "top"], key=lambda d: d["name"])
                bottom_peaks = sorted([d for d in peak_data if d["side"] == "bottom"], key=lambda d: d["name"])

                for i in range(len(filelist)):
                    if i != 0:
                        write_row = ["", str(filelist[i])]
                    else:
                        write_row = [str(g), str(filelist[i]), g_data["integration area width"]]
                        for i in range(min_nPeaks):
                            peak_detail = [top_peaks[i]["centroid"], bottom_peaks[i]["centroid"],
                                           self.average(top_peaks[i]["centroid"], bottom_peaks[i]["centroid"]),
                                           top_peaks[i]["baseline"], bottom_peaks[i]["baseline"],
                                           self.average(top_peaks[i]["baseline"], bottom_peaks[i]["baseline"]),
                                           top_peaks[i]["intensity"], bottom_peaks[i]["intensity"],
                                           self.average(top_peaks[i]["intensity"], bottom_peaks[i]["intensity"])]
                            write_row.extend(peak_detail)

                        if len(off_mer.keys()) > 0:
                            for h in ["centroid", "baseline", "intensity"]:
                                for p in [0,1]:
                                    sum_val = 0.
                                    for q in ["top_left", "top_right", "bottom_left", "bottom_right"]:
                                        write_row.append(off_mer[q][h][p])
                                        sum_val += float(off_mer[q][h][p])
                                    write_row.append(sum_val/4.)

                    writer.writerow(write_row)

    def average(self, s1, s2):
        # Find average between 2 float string. Remove '_' if necessary
        rejected = False
        if "_" in s1:
            rejected = True
            s1 = s1.lstrip("_")
        if "_" in s2:
            rejected = True
            s2 = s2.lstrip("_")

        result = str(1.*(float(s1)+float(s2))/2.)
        if rejected :
            result = "_" + result
        return result