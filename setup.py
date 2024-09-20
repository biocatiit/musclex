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
        'numpy==1.26.4',
        'Cython<=0.29.37',
        'scikit-image',
        'scikit-learn',
        'openpyxl<=3.1.2',
        'pyopencl==2024.2.7',
        'tifffile<=2023.2.28',
        'pandas<=2.2.0',
        'lmfit<=1.1.0',
        'ConfigParser<=5.3.0',
        'pillow==10.4.0',
        'fabio<=2023.10.0',
        'peakutils<=1.3.4',
        'h5py==3.10.0',
        'hdf5plugin<=4.4.0',
        'scipy>=1.11.4',
        'matplotlib==3.9.2',
        'opencv-python-headless==4.10.0.84',
        'PySide6==6.7.2',
        'pyfai<=2024.1.0',
        'distro<=1.8.0',
        'PyMca==5.9.3',
        'numba>=0.60.0',
        'fisx<=1.3.2',
        'future>=0.18.3',
        'numexpr==2.8.4',
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
