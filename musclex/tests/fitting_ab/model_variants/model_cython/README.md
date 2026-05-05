# Cython Model Variant — Build Instructions

## Requirements

- `cython` (`pip install cython` or `conda install cython`)
- `numpy`
- A C compiler (`gcc` on Linux/macOS, MSVC or MinGW on Windows)

## Build

```bash
# from the repo root, activate your environment first
conda activate musclex-test-cloud   # or your dev env

cd musclex/tests/fitting_ab/model_variants/model_cython
python setup.py build_ext --inplace
```

After a successful build you will see a file like:

```
_eval_cy.cpython-310-x86_64-linux-gnu.so   # Linux
_eval_cy.cpython-310-darwin.so             # macOS
_eval_cy.cpython-310-win_amd64.pyd         # Windows
```

## Usage

The adapter `lmfit-trf-cython` is automatically registered in the A/B
framework. If the extension is not built it raises an `ImportError` with
a message explaining the build step.

## What is compiled

Only the Gaussian kernel loop (`_eval_cy.gaussian_eval_cy`) is Cython code.
The Voigt evaluation falls back to `scipy.special.wofz` since there is no
straightforward way to call that function from Cython as a typed C function.
