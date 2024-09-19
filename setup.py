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

## Setup file to compile the sources and install the package on your system
# ==========================================
from distutils.core import setup
from setuptools import find_namespace_packages
from musclex import __version__
from distutils.extension import Extension

# Build the main package, with script etc...
# ==========================================
setup(
    name = 'musclex',
    packages = find_namespace_packages(),
    # package_dir={"": "musclex"},
    # packages = ['CalibrationSettings', 'csv_manager', 'modules', 'headless', 'ui', 'utils'],
    version = __version__,
    description = 'Muscle X',
    author = 'BioCAT',
    author_email = 'biocat@lethocerus.biol.iit.edu',
    url = 'https://github.com/biocatiit/musclex',
    keywords = ['musclex', 'biomuscle', 'diffraction', 'biocat'],
    # python_requires='>=2.7.10, <=3.6',
    classifiers = ['Development Status :: 3 - Alpha',
                   'Environment :: X11 Applications :: Qt',
                   'Intended Audience :: Science/Research',
                   'Natural Language :: English',
                   'Operating System :: Other OS',
                   'Programming Language :: Python',
                   'Programming Language :: Cython',
                   'Topic :: Scientific/Engineering :: Bio-Informatics'],
    install_requires = [
        'pip',
        'wheel',
        'numpy<1.24,>=1.18',
        'Cython<=0.29.33',
        'scikit-image',
        'scikit-learn',
        'openpyxl<=3.1.1',
        'pyopencl<=2022.3.1',
        'tifffile<=2023.2.28',
        'pandas<=1.5.3',
        'lmfit<=1.1.0',
        'ConfigParser<=5.3.0',
        'pillow<=9.4.0',
        'fabio<=2022.12.1',
        'peakutils<=1.3.4',
        'h5py<=3.8.0',
        'hdf5plugin<=4.1.1',
        'scipy<=1.10.1',
        'matplotlib<3.7,>=3.6.3',
        'opencv-python-headless<=4.7.0.72',
        'PySide6<=6.7.2',
        'pyFAI<=2023.2.0',
        'distro<=1.8.0',
        'PyMca5<=5.8.1',
        'numba<=0.57.1',
        'fisx<=1.2.1',
        'future<=0.18.3',
    ],

    entry_points={
        'console_scripts': [
            'musclex=musclex.main:main',
            'musclex-launcher=musclex.launcher:LauncherForm.main'
        ],
    },
    include_package_data=True,
    test_suite="musclex/tests"
)
