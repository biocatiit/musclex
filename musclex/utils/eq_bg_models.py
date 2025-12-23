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

from dataclasses import dataclass
import numpy as np
from typing import Dict, List, Tuple, Callable
from scipy.special import j1
from numpy.polynomial.chebyshev import chebval

Side = str  # left or right side

def side_mask_and_ndist(x: np.ndarray, centerX: float, side: Side) -> Tuple[np.ndarray, np.ndarray]:
    cx = float(centerX)
    if side == 'left':
        mask = x <= cx
        dist = (cx - x[mask])
    else:
        mask = x >= cx
        dist = (x[mask] - cx)
    if dist.size == 0:
        return mask, np.array([], dtype=float)
    dmax = max(float(dist.max()), 1.0)
    return mask, dist / dmax

@dataclass(frozen=True)
class BackgroundSpec:
    # prefix controls how parameter names are composed: left_<prefix>_<key> (if prefix), or left_<key> otherwise
    name: str
    prefix: str
    param_defs: Dict[str, Tuple[Callable[[float], float], float, float]]

    def side_param_name(self, side: Side, key: str) -> str:
        if self.prefix:
            return f"{side}_{self.prefix}_{key}"
        return f"{side}_{key}"

    def add_params(self, params, hist_max: float):
        for side in ('left', 'right'):
            for key, (init_fn, pmin, pmax) in self.param_defs.items():
                name = self.side_param_name(side, key)
                init_val = init_fn(hist_max)
                kw = {}
                if pmin is not None:
                    kw['min'] = pmin
                if pmax is not None:
                    kw['max'] = pmax
                params.add(name, init_val, **kw)

    def eval_side(self, x: np.ndarray, centerX: float, side: Side, values: Dict[str, float]) -> np.ndarray:
        raise NotImplementedError

class ExpBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A = float(values[self.side_param_name(side, 'A')])
        d = float(values[self.side_param_name(side, 'd')])
        out[mask] = A * np.exp(-d * ndist)
        return out

class Exp2Background(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A2 = float(values[self.side_param_name(side, 'A2')])
        d2 = float(values[self.side_param_name(side, 'd2')])
        out[mask] = A2 * np.exp(-d2 * ndist)
        return out

class ModLorentzBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A = float(values[self.side_param_name(side, 'A')])
        q0 = float(values[self.side_param_name(side, 'q0')])
        w = float(values[self.side_param_name(side, 'w')])
        m = float(values[self.side_param_name(side, 'm')])
        # Stable width factor
        k = 4.0 * (2.0**(1.0/m) - 1.0) / max(w**2, 1e-8)
        out[mask] = A * (1.0 / (1.0 + k * (ndist - q0)**2)**m)
        return out

class BesselBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A = float(values[self.side_param_name(side, 'A')])
        s = float(values[self.side_param_name(side, 'j1_scale')])
        j10 = float(values[self.side_param_name(side, 'j10')])
        # Avoid division by zero
        denom = s * (ndist + j10) if s != 0 else (ndist + j10 + 1e-8)
        out[mask] = A * j1(s * ndist) / denom
        return out

class PolyBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        p1 = float(values[self.side_param_name(side, 'p1')])
        p2 = float(values[self.side_param_name(side, 'p2')])
        out[mask] = p1 * ndist + p2 * (ndist**2)
        return out
    
class PearsonVIIBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A = float(values[self.side_param_name(side, 'A')])
        q0 = float(values[self.side_param_name(side, 'q0')])
        w = float(values[self.side_param_name(side, 'w')])
        m=2.0
        # Stable width factor
        k = 4.0 * (2.0**(1.0/m) - 1.0) / max(w**2, 1e-8)
        out[mask] = A * (1.0 / (1.0 + k * (ndist - q0)**2)**m)
        return out

class ChebyshevBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        
        # Get Chebyshev coefficients
        c0 = float(values[self.side_param_name(side, 'c0')])
        c1 = float(values[self.side_param_name(side, 'c1')])
        c2 = float(values[self.side_param_name(side, 'c2')])
        c3 = float(values[self.side_param_name(side, 'c3')])
        
        # Chebyshev polynomials are defined on [-1, 1], so we map ndist to this range
        # Since ndist is already normalized [0, 1], we map to [-1, 1]
        x_cheb = 2 * ndist - 1
        
        # Evaluate Chebyshev polynomial: c0*T0(x) + c1*T1(x) + c2*T2(x) + c3*T3(x)
        coeffs = [c0, c1, c2, c3]
        out[mask] = chebval(x_cheb, coeffs)
        
        return out
    
class PowerLawBackground(BackgroundSpec):
    def eval_side(self, x, centerX, side, values):
        mask, ndist = side_mask_and_ndist(x, centerX, side)
        out = np.zeros_like(x, dtype=float)
        if ndist.size == 0:
            return out
        A = float(values[self.side_param_name(side, 'A')])
        q0 = float(values[self.side_param_name(side, 'q0')])
        p = float(values[self.side_param_name(side, 'p')])
        out[mask] = A * ((ndist + q0) ** -p)
        return out

# Registry with defaults matching your current names/ranges
BACKGROUND_REGISTRY: Dict[int, BackgroundSpec] = {
    # Exp: left_exp_A, left_exp_d
    1: ExpBackground(
        name='Exp',
        prefix='exp',
        param_defs={
            'A': (lambda mx: 0.2 * mx, 0.0, None),
            'd': (lambda mx: 10, 1e-10, None),
        }
    ),
    # Exp2: second exponential; uses left_exp2_A, left_exp2_d
    2: Exp2Background(
        name='Exp2',
        prefix='exp2',
        param_defs={
            'A2':  (lambda mx: 25.0 * mx, 0.0, None),
            'd2':  (lambda mx: 115.0,     1e-6, None),
        }
    ),
    # ModLorentzian: left_A, left_q0, left_w, left_m
    3: ModLorentzBackground(
        name='ModLor',
        prefix='ml',
        param_defs={
            'A':  (lambda mx: 5.0e10 * mx, 0.0, None),
            'q0': (lambda mx: 0.0, 1e-10, None),
            'w':  (lambda mx: 1e-10, 1e-20, None),
            'm':  (lambda mx: 2.0, 0.1, 5.0),
        }
    ),
    # Bessel: left_b_A, left_b_j1_scale, left_b_j10
    4: BesselBackground(
        name='Bessel',
        prefix='b',
        param_defs={
            'A':        (lambda mx: 1.2 * mx, 0.0, None),
            'j1_scale': (lambda mx: 100.0, 1e-6, None),
            'j10':      (lambda mx: 0.0, 0.0,  None),
        }
    ),
    # Polynomial: left_p1, left_p2
    5: PolyBackground(
        name='Poly',
        prefix='p',
        param_defs={
            'p1': (lambda mx: 1.0, None, None),
            'p2': (lambda mx: 1.0, None, None),
        }
    ),
    6: PearsonVIIBackground(
        name='PearsonVII',
        prefix='',
        param_defs={
            'A':  (lambda mx: 5.0e10 * mx, 0.0, None),
            'q0': (lambda mx: 0.0, 0.0, None),
            'w':  (lambda mx: 1e-10, 1e-20, None),
        }
    ),
    # Chebyshev polynomial up to degree 3: left_cheb_c0, left_cheb_c1, left_cheb_c2, left_cheb_c3
    7: ChebyshevBackground(
        name='Chebyshev',
        prefix='cheb',
        param_defs={
            'c0': (lambda mx: 0.05 * mx, None, None),  # T0(x) = 1
            'c1': (lambda mx: -0.1 * mx, None, None),       # T1(x) = x
            'c2': (lambda mx: 0.0 * mx, None, None),       # T2(x) = 2x^2 - 1
            'c3': (lambda mx: 0.0 * mx, None, None),       # T3(x) = 4x^3 - 3x
        }
    ),
    8: PowerLawBackground(
        name='PowerLaw',
        prefix='pl',
        param_defs={
            'A': (lambda mx: 1e-6 * mx, 0.0, None),
            'q0': (lambda mx: 0.0, 1e-10, 0.1),
            'p': (lambda mx: 2.0, 1e-1, 10.0),
        }
    ),
}

def add_bg_parameters(params, hist: np.ndarray, bg_names: List[str]):
    hist_max = float(np.nanmax(hist)) if hist.size else 1.0
    # print("Histogram max value for background parameter initialization: ", hist_max, type(hist_max))
    for name in bg_names:
        BACKGROUND_REGISTRY[name].add_params(params, hist_max)

def eval_backgrounds(bg_names: List[int], x: np.ndarray, centerX: float, kwargs: Dict[str, float]) -> np.ndarray:
    if len(bg_names) == 0:
        return np.zeros_like(x, dtype=float)
    out = np.zeros_like(x, dtype=float)
    for name in bg_names:
        spec = BACKGROUND_REGISTRY[name]
        out += spec.eval_side(x, centerX, 'left', kwargs)
        out += spec.eval_side(x, centerX, 'right', kwargs)
    return out