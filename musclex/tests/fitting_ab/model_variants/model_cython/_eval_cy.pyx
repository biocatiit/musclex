# cython: language_level=3
# cython: boundscheck=False
# cython: wraparound=False
# cython: cdivision=True
"""
Cython C-extension: fast Gaussian evaluation for A/B benchmarking.

Compile with::

    cd musclex/tests/fitting_ab/model_variants/model_cython
    python setup.py build_ext --inplace

The resulting ``_eval_cy*.so`` (Linux/macOS) or ``_eval_cy*.pyd`` (Windows)
is then importable as ``from _eval_cy import gaussian_eval_cy``.

Voigt is intentionally left as a Python/NumPy function because Cython cannot
call ``scipy.special.wofz`` as a typed C function without significant extra
setup.  Since Voigt is rarely the bottleneck this is an acceptable trade-off.
"""

import numpy as np
cimport numpy as cnp
from libc.math cimport exp, sqrt

cdef double _SQRT_2PI = sqrt(2.0 * 3.141592653589793)


def gaussian_eval_cy(
    cnp.ndarray[cnp.float64_t, ndim=1] x,
    double amplitude,
    double center,
    double sigma,
):
    """Area-normalised Gaussian (matches lmfit GaussianModel convention).

    Parameters match ``_gaussian_eval(x, amplitude, center, sigma)`` in the
    other model variants so it can be swapped in without changing call sites.
    """
    cdef int n = x.shape[0]
    cdef cnp.ndarray[cnp.float64_t, ndim=1] result = np.empty(n, dtype=np.float64)
    cdef double factor = amplitude / (sigma * _SQRT_2PI)
    cdef double dz
    cdef int i

    for i in range(n):
        dz = (x[i] - center) / sigma
        result[i] = factor * exp(-0.5 * dz * dz)

    return result
