# from distutils.core import setup
from setuptools import setup, find_packages
from musclex import __version__
from distutils.extension import Extension
from Cython.Distutils import build_ext
import numpy

ext_modules=[ Extension("musclex.biocat_modules.QF_utilities",
              ["musclex/biocat_modules/QF_utilities.pyx"],
              libraries=["m"],
              extra_compile_args = ["-ffast-math"])]

setup(
    name = 'musclex',
    packages = find_packages(),
    # package_dir={"musclex": "musclex"},
    version = __version__,
    description = 'Muscle X',
    author = 'Jiranun J.',
    author_email = 'jjiratra@hawk.iit.edu',
    url = 'https://github.com/biocatiit/musclex',
    download_url = 'https://github.com/biocatiit/musclex/archive/0.1.tar.gz',
    keywords = ['musclex', 'biomuscle', 'diffraction'],
    python_requires='>=2.7.10, <=2.7.12',
    classifiers = ['Development Status :: 3 - Alpha',
                   'Environment :: X11 Applications :: Qt',
                   'Intended Audience :: Science/Research',
                   'Natural Language :: English',
                   'Operating System :: Other OS',
                   'Programming Language :: Python :: 2.7',
                   'Programming Language :: Cython'],
    install_requires=['scikit-image',
                      'tifffile',
                      'numpy',
                      'pandas',
                      'scikit-learn',
                      'lmfit',
                      'ConfigParser',
                      'pillow',
                      'cython',
                      'fabio',
                      'peakutils',
                      'h5py',
                      'scipy',
                      'matplotlib'],
    entry_points={
        'console_scripts': [
            'musclex=musclex.main:main',
        ],
    },
    ext_modules=ext_modules,
    cmdclass={"build_ext": build_ext},
    include_dirs=[numpy.get_include()]
)
