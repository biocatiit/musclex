__author__ = 'Jiranun.J'

from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import numpy

ext_modules=[ Extension("QF_utilities",
              ["QF_utilities.pyx"],
              libraries=["m"],
              extra_compile_args = ["-ffast-math"])]

setup(
    name='Utilities for Quadrant-Folding',
    ext_modules=ext_modules,
    cmdclass = {"build_ext": build_ext},
    include_dirs=[numpy.get_include()]
)

#
# from distutils.core import setup
# from Cython.Build import cythonize
# import numpy
#
# setup(
#     name='Utilities for Quadrant-Folding',
#     ext_modules=cythonize("QF_utilities.pyx"),
#     include_dirs=[numpy.get_include()]
# )
