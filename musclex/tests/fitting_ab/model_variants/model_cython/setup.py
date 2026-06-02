"""
Build the Cython extension for the model_cython variant.

Run from this directory::

    python setup.py build_ext --inplace

Or use the helper script::

    python -m musclex.tests.fitting_ab.model_variants.model_cython.build

After building you will see a file named ``_eval_cy.cpython-3XX-*.so``
(Linux/macOS) or ``_eval_cy.cpython-3XX-*.pyd`` (Windows) in this directory.
"""

from setuptools import setup, Extension

try:
    import numpy as np
    from Cython.Build import cythonize

    ext = Extension(
        name="_eval_cy",
        sources=["_eval_cy.pyx"],
        include_dirs=[np.get_include()],
        extra_compile_args=["-O2", "-fno-tree-vectorize"],
    )

    setup(
        name="musclex_fitting_ab_eval_cy",
        ext_modules=cythonize(
            [ext],
            compiler_directives={
                "language_level": "3",
                "boundscheck": False,
                "wraparound": False,
                "cdivision": True,
            },
        ),
    )

except ImportError as exc:
    raise SystemExit(
        f"Build requires cython and numpy: {exc}\n"
        "Install with: pip install cython numpy"
    ) from exc
