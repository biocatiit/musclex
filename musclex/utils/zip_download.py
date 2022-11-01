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

import shutil
import os
import sys
from musclex import __version__
from io import BytesIO
from urllib.request import urlopen
from zipfile import ZipFile

def download_zip_pickles(dirpath):
    """
    Download the pickle files from SourceForge, unzip them and put them in the right folder for testing
    """
    if getattr(sys, 'frozen', False):
        direc_path = dirpath
    else:
        direc_path = os.path.join(dirpath, "tests")
    url = "https://sourceforge.net/projects/musclex/files/pickle_tests_v" + __version__ + ".zip/download"
    if os.path.exists(os.path.join(direc_path, "di")):
        print("Pickle files have already been downloaded.")
    else:
        print("Downloading and unzipping pickle files for testing...")
        try:
            with urlopen(url) as zipresp:
                with ZipFile(BytesIO(zipresp.read())) as zfile:
                    zfile.extractall(direc_path)
        except Exception:
            print("Error during downloading or unzipping, check your internet connection and retry.")

        print("Moving files to testing folder...")
        shutil.move(os.path.join(direc_path, "pickle_tests_v" + __version__, "di"),
                    os.path.join(direc_path, "di"))
        shutil.move(os.path.join(direc_path, "pickle_tests_v" + __version__, "dc"),
                    os.path.join(direc_path, "dc"))
        shutil.move(os.path.join(direc_path, "pickle_tests_v" + __version__, "eq"),
                    os.path.join(direc_path, "eq"))
        shutil.move(os.path.join(direc_path, "pickle_tests_v" + __version__, "qf"),
                    os.path.join(direc_path, "qf"))
        shutil.move(os.path.join(direc_path, "pickle_tests_v" + __version__, "pt"),
                    os.path.join(direc_path, "pt"))
        print("Cleaning download files...")
        if os.path.exists(os.path.join(dirpath, "tests", "pickle_tests_v" + __version__)):
            shutil.rmtree(os.path.join(dirpath, "tests", "pickle_tests_v" + __version__))
        print("Done.")
