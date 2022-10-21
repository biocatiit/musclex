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

import dload
import shutil
import os

def download_zip_pickles(dirpath):
    """
    Download the pickle files from SourceForge, unzip them and put them in the right folder for testing
    """
    if os.path.exists(os.path.join(dirpath, "tests", "di")):
        print("Pickle files have already been downloaded.")
    else:
        print("Downloading and unzipping pickle files for testing...")
        try:
            dload.save_unzip("https://sourceforge.net/projects/musclex/files/pickle_tests.zip/download", 
                            os.path.join(dirpath, "tests"))
        except Exception:
            print("Error during downloading or unzipping, check your internet connection and retry.")

        print("Moving files to testing folder...")
        shutil.move(os.path.join(dirpath, "tests", "pickle_tests", "di"),
                    os.path.join(dirpath, "tests", "di"))
        shutil.move(os.path.join(dirpath, "tests", "pickle_tests", "dc"),
                    os.path.join(dirpath, "tests", "dc"))
        shutil.move(os.path.join(dirpath, "tests", "pickle_tests", "eq"),
                    os.path.join(dirpath, "tests", "eq"))
        shutil.move(os.path.join(dirpath, "tests", "pickle_tests", "qf"),
                    os.path.join(dirpath, "tests", "qf"))
        shutil.move(os.path.join(dirpath, "tests", "pickle_tests", "pt"),
                    os.path.join(dirpath, "tests", "pt"))

        print("Cleaning download files...")
        if os.path.exists(os.path.join(dirpath, "tests", "pickle_tests")):
            shutil.rmtree(os.path.join(dirpath, "tests", "pickle_tests"))
        if os.path.exists(os.path.join(dirpath, "tests", "download")):
            os.remove(os.path.join(dirpath, "tests", "download"))
        print("Done.")
