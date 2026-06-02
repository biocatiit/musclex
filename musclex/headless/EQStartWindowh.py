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

import sys
import os
from os.path import split

try:
    from ..headless.EquatorWindowh import EquatorWindowh
    from ..utils.file_manager import getImgFiles
except:  # for coverage
    from headless.EquatorWindowh import EquatorWindowh
    from utils.file_manager import getImgFiles


def _resolve_worker_limit():
    """
    Resolve the maximum number of concurrent worker processes for
    headless Equator batch processing.

    Why this exists:
        When MuscleX is integrated into beamline control software
        running on the same data server, we need a reliable way to
        reserve cores for other services (detector readout, control
        loops, file I/O, etc.). Without this, the headless batch path
        would always spawn `cpu_count()` workers, which can starve
        co-tenant processes.

    Resolution order:
        1. ``MUSCLEX_WORKERS`` environment variable (must be a
           positive integer). This matches the convention already used
           by ``EquatorWindow`` (GUI mode), so a single env var
           controls both code paths.
        2. ``cpu_count() - 2`` as a default, leaving 2 logical CPUs as
           headroom for the OS/GUI/beamline services.
        3. ``1`` as the absolute minimum to ensure forward progress on
           single-core or misconfigured machines.

    Returns:
        int: positive integer, the maximum number of Equator worker
        processes allowed to run concurrently.
    """
    from multiprocessing import cpu_count

    env_val = os.environ.get("MUSCLEX_WORKERS")
    if env_val is not None:
        try:
            parsed = int(env_val)
            if parsed >= 1:
                return parsed
        except (TypeError, ValueError):
            print(
                f"[eq -h] Ignoring invalid MUSCLEX_WORKERS={env_val!r}; "
                f"falling back to default"
            )
    try:
        return max(1, (cpu_count() or 2) - 2)
    except NotImplementedError:
        return 1


class EQStartWindowh:
    """
    A class for start-up window or main window. Now, this is used for keep all EquatorWindow objects in a list
    """

    def __init__(
        self, filename, inputsettings, delcache, settingspath, output_dir=None
    ):

        self.dir_path = filename
        self.inputFlag = inputsettings
        self.delcache = delcache
        self.settingspath = settingspath
        self.output_dir = output_dir
        is_hdf5 = os.path.splitext(self.dir_path)[1] in [".h5", ".hdf5", ".txt"]
        if os.path.isfile(self.dir_path) and not is_hdf5:
            self.browseFile()  # start program by browse a file
        elif os.path.isdir(self.dir_path) or is_hdf5:
            self.browseFolder(is_hdf5)
        else:
            print("Can't load image file or folder")
            return

    def browseFolder(self, is_hdf5=False):
        """
        Popup an input folder dialog. Users can select a folder
        """
        input_types = [
            ".adsc",
            ".cbf",
            ".edf",
            ".fit2d",
            ".mar345",
            ".marccd",
            ".pilatus",
            ".tif",
            ".tiff",
            ".smv",
        ]
        from multiprocessing import Lock, Process

        # Cap concurrent workers via MUSCLEX_WORKERS so beamline
        # control software co-tenants can reserve cores. The original
        # implementation hard-coded `cpu_count()` here, which always
        # saturated the host. See `_resolve_worker_limit` for details.
        worker_limit = _resolve_worker_limit()
        print(
            f"[eq -h] Using up to {worker_limit} parallel worker process(es) "
            f"(override with MUSCLEX_WORKERS=<n>)"
        )

        lock = Lock()
        procs = []
        if self.dir_path != "":
            imgList = os.listdir(self.dir_path) if not is_hdf5 else [self.dir_path]
        for image in imgList:
            file_name = (
                os.path.join(self.dir_path, image) if not is_hdf5 else self.dir_path
            )
            if os.path.isfile(file_name):
                _, ext = os.path.splitext(str(file_name))
                if ext in input_types:
                    print("filename is", file_name)
                    if self.settingspath == "empty":
                        proc = Process(
                            target=EquatorWindowh,
                            args=(file_name, self.inputFlag, self.delcache, lock),
                            kwargs={"output_dir": self.output_dir},
                        )
                    else:
                        proc = Process(
                            target=EquatorWindowh,
                            args=(
                                file_name,
                                self.inputFlag,
                                self.delcache,
                                lock,
                                None,
                                None,
                                None,
                                None,
                                None,
                                self.settingspath,
                            ),
                            kwargs={"output_dir": self.output_dir},
                        )
                    procs.append(proc)
                    proc.start()
                elif ext in [".h5", ".hdf5", ".txt"]:
                    hdir_path, himgList, _, hfileList, _ = getImgFiles(
                        str(file_name), headless=True
                    )
                    for ind in range(len(himgList)):
                        print("filename is", himgList[ind])
                        if self.settingspath == "empty":
                            proc = Process(
                                target=EquatorWindowh,
                                args=(
                                    file_name,
                                    self.inputFlag,
                                    self.delcache,
                                    lock,
                                    hdir_path,
                                    himgList,
                                    ind,
                                    hfileList,
                                    ext,
                                ),
                                kwargs={"output_dir": self.output_dir},
                            )
                        else:
                            proc = Process(
                                target=EquatorWindowh,
                                args=(
                                    file_name,
                                    self.inputFlag,
                                    self.delcache,
                                    lock,
                                    hdir_path,
                                    himgList,
                                    ind,
                                    hfileList,
                                    ext,
                                    self.settingspath,
                                ),
                                kwargs={"output_dir": self.output_dir},
                            )
                        procs.append(proc)
                        proc.start()
                        if len(procs) >= worker_limit:
                            for proc in procs:
                                proc.join()
                            procs = []
            if len(procs) >= worker_limit:
                for proc in procs:
                    proc.join()
                procs = []
        for proc in procs:
            proc.join()
        # self.runBioMuscle(file_name)

    def browseFile(self):
        """
        Popup an input file dialog. Users can select an image or .txt for failed cases list
        """
        file_name = self.dir_path
        _, ext = os.path.splitext(str(file_name))
        _, name = split(str(file_name))
        if file_name != "":
            if ext == ".txt" and not name == "failedcases.txt":
                print("Please select only failedcases.txt or image files\n")
                self.browseFile()
            else:
                # Run BioMuscle if the file is an image or failed cases list
                self.runBioMuscle(str(file_name))
        else:
            sys.exit()

    def runBioMuscle(self, filename):
        """
        Create a EquatorWindow object and launch the window
        :param filename: input filename (str)
        :return:
        """
        settingspath = self.settingspath
        if settingspath is None:
            EquatorWindowh(
                filename, self.inputFlag, self.delcache, output_dir=self.output_dir
            )
        else:
            EquatorWindowh(
                filename,
                self.inputFlag,
                self.delcache,
                settingspath=settingspath,
                output_dir=self.output_dir,
            )
