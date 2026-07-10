import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import least_squares
from skimage.transform import resize


class EllipticalSAXSFitter:
    """
    Fits 2D SAXS patterns using elliptical cylindrical coordinates
    as described in Murthy et al., Polymer 1997.
    """

    def __init__(
        self,
        pattern,
        crop=[None, None],
        center=None,
        mask=None,
        model="elliptical",
        n_peaks=1,
        downsample_factor=1,
        K=1,
        kernel="exponential",
        comp1=None,
        comp2=None,
        comp3=None,
        aspect=False,
    ):
        """
        Initialize the fitter with a 2D pattern.
        """
        self.downsample_factor = downsample_factor

        # Downsample the pattern if downsample_factor > 1
        if downsample_factor > 1:
            print(f"Downsampling pattern by factor {downsample_factor}...")
            pattern = resize(
                pattern,
                (
                    pattern.shape[0] // downsample_factor,
                    pattern.shape[1] // downsample_factor,
                ),
                anti_aliasing=True,
            )
            if mask is not None:
                mask = resize(
                    mask,
                    (
                        mask.shape[0] // downsample_factor,
                        mask.shape[1] // downsample_factor,
                    ),
                    anti_aliasing=False,
                ).astype(bool)

        self.pattern_full = pattern.astype(float)
        self.pattern = pattern.astype(float)
        self.ny, self.nx = pattern.shape

        if crop[0] is not None or crop[1] is not None:
            y0 = pattern.shape[0] // 2
            x0 = pattern.shape[1] // 2
            y_min = y0 - crop[0] // 2 if crop[0] is not None else 0
            y_max = y0 + crop[0] // 2 if crop[0] is not None else pattern.shape[0]
            x_min = x0 - crop[1] // 2 if crop[1] is not None else 0
            x_max = x0 + crop[1] // 2 if crop[1] is not None else pattern.shape[1]
            pattern = pattern[y_min:y_max, x_min:x_max]
        self.pattern = pattern.astype(float)
        self.ny, self.nx = pattern.shape

        self.mask = None
        if mask is not None:
            m = np.asarray(mask, dtype=bool)
            if m.shape != self.pattern.shape:
                raise ValueError("mask shape must match pattern")
            self.mask = m

        if center is None:
            # True pixel center: grid is np.arange(nx), so pixel centers run
            # 0..nx-1 and the geometric center is (nx-1)/2. Using nx/2 here
            # would sit half a pixel off in both parities, which shows up as a
            # spurious asymmetry in radial projections (worst for sharp kernels).
            self.x0 = (self.nx - 1) / 2.0
            self.y0 = (self.ny - 1) / 2.0
        else:
            self.x0, self.y0 = center

        x = np.arange(self.nx)
        y = np.arange(self.ny)
        self.X, self.Y = np.meshgrid(x, y)

        # Shift to center
        self.X_centered = self.X - self.x0
        self.Y_centered = self.Y - self.y0
        # Precompute radial distance for circular model
        self.R = np.hypot(self.X_centered, self.Y_centered)
        # Precompute polar angles (radians & degrees) for circular / anisotropic model
        self.THETA = np.arctan2(self.Y_centered, self.X_centered)  # (-pi, pi]
        self.THETA_deg = (np.degrees(self.THETA) + 360.0) % 360.0
        self.model = model
        self.n_peaks = n_peaks
        self.circular_order = K
        self.circular_kernel = kernel
        # Elliptical-distance toggle for the circular model. When True, the
        # isotropic radius R = sqrt(x^2 + y^2) is replaced by the elliptical
        # radius R_ell = sqrt((x/e)^2 + y^2), adding one aspect-ratio parameter
        # `e` (e>1 => stretched horizontally). When False, model is strictly
        # circular and `e` is absent from the parameter vector.
        self.aspect = aspect
        # Explicit two-component selectors (preferred over circular_kernel when
        # set). comp1=None => fall back to decoding the legacy `kernel` string.
        self.comp1_kernel = comp1
        self.comp2_kernel = comp2
        self.comp3_kernel = comp3
        if comp1 is not None:
            self.set_components(comp1, comp2, comp3)

    def modified_lorentzian(self, x, x0, width, m=2):
        """
        Modified Lorentzian (Pearson VII with m=2).
        """
        if width <= 0:
            width = 1e-6
        k = 4.0 * (2.0 ** (1.0 / m) - 1.0) / (width**2)
        return 1.0 / (1.0 + k * (x - x0) ** 2) ** m

    def cartesian_to_polar_np(self, x, y, degrees=False, normalize=False):
        """
        Vectorized version for a NumPy array of shape (N, 2) -> returns array of shape (N, 2) with (r, theta).
        """
        r = np.hypot(x, y)
        theta = np.arctan2(y, x)
        if degrees:
            theta = np.degrees(theta)
            if normalize:
                theta = (theta % 360.0 + 360.0) % 360.0
        else:
            if normalize:
                theta = (theta % (2 * np.pi) + 2 * np.pi) % (2 * np.pi)
        return r, theta

    def cartesian_to_elliptic_np(self, x, y, A, degrees=False, normalize=False):
        """
        Cartesian (x, y) -> elliptical (u, v) for
            x = sqrt(A^2 + u^2) * cos(v)    (Eq. 11)
            y = u * sin(v)                  (Eq. 12)
        with A = 2c (Eq. 13), u >= 0, v in (-pi, pi].

        Inversion:
        Let r^2 = x^2 + y^2 and t = u^2. From Eqs. (11–12):
            t^2 + (A^2 - r^2) t - (A^2 y^2) = 0
        Choose the nonnegative root:
            u^2 = 0.5 * (r^2 - A^2 + sqrt((r^2 - A^2)^2 + 4 A^2 y^2))
        """
        if A <= 0:
            raise ValueError("A must be > 0 (distance between foci).")

        x = np.asarray(x, dtype=float)
        y = np.asarray(y, dtype=float)

        r2 = x * x + y * y
        A2 = A * A
        disc = (r2 - A2) ** 2 + 4.0 * A2 * y * y
        t = 0.5 * (r2 - A2 + np.sqrt(disc))
        t = np.maximum(t, 0.0)
        u = np.sqrt(t)

        # Compute v from sin and cos for correct quadrant
        denom_c = np.sqrt(A2 + u * u)  # > 0 because A > 0
        cos_v = np.clip(x / denom_c, -1.0, 1.0)

        sin_v = np.zeros_like(cos_v)
        nz = u > 0
        sin_v[nz] = np.clip(y[nz] / u[nz], -1.0, 1.0)  # for u=0, leave sin_v=0

        v = np.arctan2(sin_v, cos_v)  # (-pi, pi]

        if degrees:
            v = np.degrees(v)
            if normalize:
                v = (v % 360.0 + 360.0) % 360.0
        else:
            if normalize:
                two_pi = 2.0 * np.pi
                v = (v % two_pi + two_pi) % two_pi
        return u, v

    def elliptical_intensity(self, params, return_components=False):
        """
        Calculate intensity in elliptical coordinates.

        Parameters:
        -----------
        params : array-like
            [A, baseline, amp1, u01, uw1, v01, vw1, m
             amp2, u02, uw2, v02, vw2, m
             amp3, u03, uw3, v03, vw3, m ]
        """
        A = np.abs(params[0])
        baseline = max(params[1], 0)

        # Use normalized degrees to [0, 360) for consistent angular wrapping
        u, v = self.cartesian_to_elliptic_np(
            self.X_centered, self.Y_centered, A, degrees=True, normalize=False
        )
        # u, v = self._elliptical_coords

        intensity = np.ones_like(u) * baseline

        # params layout: [A, baseline, (amp, u0, uw, v0, vw)*n]
        n_peaks = (len(params) - 2) // 6
        components = []
        eps = 1e-6

        for i in range(n_peaks):
            idx = 2 + i * 6
            amp = params[idx]
            u0 = params[idx + 1]
            uw = max(np.abs(params[idx + 2]), eps)
            v0 = params[idx + 3]
            vw = max(np.abs(params[idx + 4]), eps)
            m = params[idx + 5]  # modified Lorentzian parameter

            f_u = self.modified_lorentzian(u, u0, uw, m)

            # 4 symmetric angular positions
            g_v = np.zeros_like(v)
            v_positions = [
                v0 % 360.0,
                (180.0 - v0) % 360.0,
                (180.0 + v0) % 360.0,
                (360.0 - v0) % 360.0,
            ]
            for v_i in v_positions:
                v_diff = v - v_i
                # wrap to (-180, 180]
                v_diff = np.where(v_diff > 180.0, v_diff - 360.0, v_diff)
                v_diff = np.where(v_diff <= -180.0, v_diff + 360.0, v_diff)
                g_v += self.modified_lorentzian(v_diff, 0.0, vw)

            g_v *= 0.25

            peak_intensity = amp * f_u * g_v
            intensity += peak_intensity

            if return_components:
                components.append(peak_intensity)

        intensity = np.maximum(intensity, 0.0)

        if return_components:
            return intensity, components
        return intensity

    # ---- Anisotropic circular model helpers ----
    def _fourier_series(self, theta, a0, cos_coeffs, sin_coeffs):
        """
        theta: array (radians)
        a0: scalar
        cos_coeffs, sin_coeffs: length K each
        """
        val = a0
        for k, (cc, ss) in enumerate(zip(cos_coeffs, sin_coeffs), start=1):
            if cc != 0:
                val += cc * np.cos(k * theta)
            if ss != 0:
                val += ss * np.sin(k * theta)
        return val

    @staticmethod
    def _softplus(x):
        """Numerically stable softplus -> strictly positive output."""
        return np.logaddexp(0.0, x) + 1e-6

    def _radial_shape(self, R, scale, kernel, beta=1.0, n=4.0):
        """
        Radial shape function s(R) with s(0)=1, monotonically decaying.
        `scale` is a per-pixel (broadcastable) positive length scale.

        kernel:
            'exponential' : exp(-R/scale)
            'gaussian'    : exp(-0.5*(R/scale)**2)          (scale = sigma, fast decay)
            'stretched'   : exp(-(R/scale)**beta)          (beta>0; <1 = fat tail)
            'lorentzian'  : 1 / (1 + (R/scale)**2)         (Ornstein-Zernike, tail ~R^-2)
            'lorentzian2' : 1 / (1 + (R/scale)**2)**2      (Debye-Bueche, tail ~R^-4)
            'powerlaw'    : (1 + R/scale)**(-n)            (Porod/fractal, soft core)
        """
        if kernel == "exponential":
            return np.exp(-R / scale)
        elif kernel == "gaussian":
            return np.exp(-0.5 * (R / scale) ** 2)
        elif kernel == "stretched":
            beta = max(float(beta), 1e-3)
            return np.exp(-((R / scale) ** beta))
        elif kernel == "lorentzian":
            return 1.0 / (1.0 + (R / scale) ** 2)
        elif kernel == "lorentzian2":
            return 1.0 / (1.0 + (R / scale) ** 2) ** 2
        elif kernel == "powerlaw":
            n = max(float(n), 1e-3)
            return (1.0 + R / scale) ** (-n)
        else:
            raise ValueError(f"Unknown circular kernel '{kernel}'")

    # ---- Two-component kernel registry -----------------------------------
    # Radial shapes valid for either component.
    _BASE_RADIAL = (
        "exponential",
        "gaussian",
        "stretched",
        "lorentzian",
        "lorentzian2",
        "powerlaw",
    )
    # comp2-only special shapes -> number of trailing parameters they consume.
    _COMP2_SPECIAL_NPARAM = {
        "linear": 1,  # slope
        "cheb2": 2,  # c1, c2
        "cheb": 3,  # c1, c2, c3
        "cheb4": 4,  # c1, c2, c3, c4
        "moffat": 3,  # amp2, gamma_raw, beta
        "parzen": 2,  # amp2, n
    }
    # Legacy single-string `circular_kernel` -> (comp1, comp2) for back-compat.
    _LEGACY_KERNEL_MAP = {
        "exponential": ("exponential", None),
        "stretched": ("stretched", None),
        "lorentzian": ("lorentzian", None),
        "lorentzian2": ("lorentzian2", None),
        "powerlaw": ("powerlaw", None),
        "double_exp": ("exponential", "exponential"),
        "exp_linear": ("exponential", "linear"),
        "exp_cheb": ("exponential", "cheb"),
        "exp_cheb2": ("exponential", "cheb2"),
        "exp_cheb4": ("exponential", "cheb4"),
        "exp_moffat": ("exponential", "moffat"),
        "exp_parzen": ("exponential", "parzen"),
    }

    @classmethod
    def _radial_extra_count(cls, kernel):
        """Number of trailing shape params a base radial kernel consumes."""
        return 1 if kernel in ("stretched", "powerlaw") else 0

    def _resolve_components(self):
        """Return (comp1_kernel, comp2_kernel, comp3_kernel).

        Prefers the explicit `self.comp1_kernel` / `self.comp2_kernel` /
        `self.comp3_kernel` selectors (set via `set_components` or
        `fit(comp1=..., comp2=..., comp3=...)`). Falls back to decoding the
        legacy single `self.circular_kernel` string (which has no comp3).
        """
        c1 = getattr(self, "comp1_kernel", None)
        if c1 is not None:
            c2 = getattr(self, "comp2_kernel", None)
            c3 = getattr(self, "comp3_kernel", None)
            if c1 not in self._BASE_RADIAL:
                raise ValueError(
                    f"comp1 must be one of {self._BASE_RADIAL}, got '{c1}'"
                )
            for name, c in (("comp2", c2), ("comp3", c3)):
                if (
                    c is not None
                    and c not in self._BASE_RADIAL
                    and c not in self._COMP2_SPECIAL_NPARAM
                ):
                    raise ValueError(f"unknown {name} kernel '{c}'")
            return c1, c2, c3
        legacy = getattr(self, "circular_kernel", "exponential")
        if legacy not in self._LEGACY_KERNEL_MAP:
            raise ValueError(f"Unknown circular_kernel '{legacy}'")
        c1, c2 = self._LEGACY_KERNEL_MAP[legacy]
        return c1, c2, None

    def _comp2_param_count(self, kernel):
        """Number of trailing parameters the chosen comp2 kernel consumes."""
        if kernel is None:
            return 0
        if kernel in self._BASE_RADIAL:
            # amp2, scale2_raw, plus a shape param for stretched/powerlaw.
            return 2 + self._radial_extra_count(kernel)
        return self._COMP2_SPECIAL_NPARAM[kernel]

    def _build_comp2(self, kernel, extras, R, R_edge):
        """Evaluate the second component on the grid from its parameter slice.

        Base radial kernels are isotropic (their own amp2 + length scale, like
        the legacy double_exp) and edge-zeroed so they cannot eat the baseline.
        The special shapes are centered backgrounds constrained at the edge.
        """
        if kernel in self._BASE_RADIAL:
            amp2 = max(extras[0], 0.0) if len(extras) >= 1 else 0.0
            D2 = self._softplus(extras[1]) if len(extras) >= 2 else 1.0
            beta2 = (
                max(extras[2], 1e-3)
                if (kernel == "stretched" and len(extras) >= 3)
                else 1.0
            )
            n2 = (
                max(extras[2], 1e-3)
                if (kernel == "powerlaw" and len(extras) >= 3)
                else 4.0
            )
            sR = self._radial_shape(R, D2, kernel, beta=beta2, n=n2)
            sE = self._radial_shape(R_edge, D2, kernel, beta=beta2, n=n2)
            return amp2 * (sR - sE)

        if kernel == "linear":
            # Circular linear cone: max at center, decays linearly to 0 at edge.
            slope = max(extras[0], 0.0) if len(extras) >= 1 else 0.0
            return slope * (R_edge - R)

        if kernel in ("cheb", "cheb2", "cheb4"):
            # Radial Chebyshev poly in R (orders 1..deg). Order-0 omitted: the
            # shared baseline already plays that role. R->x in [-1, 1].
            x = 2.0 * R / R_edge - 1.0
            T = [
                x,
                2.0 * x * x - 1.0,
                4.0 * x**3 - 3.0 * x,
                8.0 * x**4 - 8.0 * x**2 + 1.0,
            ]
            ncoef = self._COMP2_SPECIAL_NPARAM[kernel]
            comp2 = 0.0
            for i in range(ncoef):
                ci = extras[i] if len(extras) > i else 0.0
                comp2 = comp2 + ci * T[i]
            return comp2

        if kernel == "moffat":
            # m(R) = amp2 * [1 + (R/gamma)^2]^-beta, edge-zeroed. Power-law tails,
            # monotone from center, no oscillation.
            amp2 = max(extras[0], 0.0) if len(extras) >= 1 else 0.0
            gamma = self._softplus(extras[1]) if len(extras) >= 2 else 1.0
            beta2 = max(extras[2], 1e-3) if len(extras) >= 3 else 2.0
            m_R = (1.0 + (R / gamma) ** 2) ** (-beta2)
            m_edge = (1.0 + (R_edge / gamma) ** 2) ** (-beta2)
            return amp2 * (m_R - m_edge)

        if kernel == "parzen":
            # p(R) = amp2 * (1 - (R/R_edge)^2)^n. Polynomial, smooth, monotone,
            # identically 0 at the edge (hard end constraint).
            amp2 = max(extras[0], 0.0) if len(extras) >= 1 else 0.0
            n_pz = max(extras[1], 1e-3) if len(extras) >= 2 else 2.0
            base = np.clip(1.0 - (R / R_edge) ** 2, 0.0, None)
            return amp2 * base**n_pz

        raise ValueError(f"Unknown comp2 kernel '{kernel}'")

    def _default_comp_params(self, kernel, amp0, d0, r_max, d_raw_hi):
        """Initial parameter values for an optional component (comp2/comp3).

        Mirrors `_build_comp2`'s layout for each kernel. Returns an empty list
        when `kernel` is None.
        """
        if kernel is None:
            return []
        if kernel in self._BASE_RADIAL:
            # Second/third isotropic radial term: smaller amp, longer scale.
            amp_0 = 0.3 * amp0
            d2 = min(3.0 * d0, r_max)
            d2_raw = np.log(np.expm1(d2)) if d2 > 1 else d2
            d2_raw = min(d2_raw, d_raw_hi - 1e-3)
            out = [amp_0, d2_raw]
            if kernel == "stretched":
                out.append(1.0)  # beta
            elif kernel == "powerlaw":
                out.append(4.0)  # n
            return out
        if kernel == "linear":
            return [0.3 * amp0 / max(r_max, 1.0)]  # slope
        if kernel == "cheb":
            return [0.0, 0.0, 0.0]  # c1, c2, c3 (flat start)
        if kernel == "cheb2":
            return [0.0, 0.0]  # c1, c2
        if kernel == "cheb4":
            return [0.0, 0.0, 0.0, 0.0]  # c1..c4
        if kernel == "moffat":
            gamma0 = max(0.5 * r_max, 1.0)
            gamma_raw = np.log(np.expm1(gamma0)) if gamma0 > 1 else gamma0
            return [0.3 * amp0, gamma_raw, 2.0]  # amp, gamma_raw, beta
        if kernel == "parzen":
            return [0.3 * amp0, 2.0]  # amp, n
        raise ValueError(f"Unknown component kernel '{kernel}'")

    def _comp_param_bounds(self, kernel, d_raw_lo, d_raw_hi):
        """(lower, upper) bound lists for an optional component (comp2/comp3)."""
        if kernel is None:
            return [], []
        if kernel in self._BASE_RADIAL:
            lo = [0.0, d_raw_lo]  # amp, scale_raw
            hi = [np.inf, d_raw_hi]
            if kernel == "stretched":
                lo += [0.1]
                hi += [3.0]  # beta
            elif kernel == "powerlaw":
                lo += [0.5]
                hi += [8.0]  # n
            return lo, hi
        if kernel == "linear":
            return [0.0], [np.inf]  # slope
        if kernel == "cheb":
            return [-np.inf] * 3, [np.inf] * 3  # c1, c2, c3 (signed)
        if kernel == "cheb2":
            return [-np.inf] * 2, [np.inf] * 2  # c1, c2 (signed)
        if kernel == "cheb4":
            return [-np.inf] * 4, [np.inf] * 4  # c1..c4 (signed)
        if kernel == "moffat":
            return [0.0, d_raw_lo, 0.5], [np.inf, d_raw_hi, 8.0]  # amp, gamma_raw, beta
        if kernel == "parzen":
            return [0.0, 0.5], [np.inf, 12.0]  # amp, n
        raise ValueError(f"Unknown component kernel '{kernel}'")

    def _print_comp_params(self, kernel, extras, label="Comp2"):
        """Pretty-print one optional component's fitted parameters."""
        if kernel is None:
            return
        if kernel in self._BASE_RADIAL and len(extras) >= 2:
            D2 = np.log1p(np.exp(extras[1])) + 1e-6
            print(f"{label} kernel:       {kernel}")
            print(f"{label} amp:          {extras[0]:.2f} counts")
            print(f"{label} scale (eff):  {D2:.2f} pixels")
            if kernel == "stretched" and len(extras) >= 3:
                print(f"{label} beta:         {extras[2]:.3f}")
            elif kernel == "powerlaw" and len(extras) >= 3:
                print(f"{label} n:            {extras[2]:.3f}")
        elif kernel == "linear" and len(extras) >= 1:
            R_edge = float(self.R.max())
            print(f"{label} linear slope: {extras[0]:.4f} counts/pixel")
            print(f"{label} linear peak:  {extras[0] * R_edge:.2f} counts (at center)")
        elif kernel in ("cheb", "cheb2", "cheb4"):
            ncoef = self._COMP2_SPECIAL_NPARAM[kernel]
            for i in range(min(ncoef, len(extras))):
                print(f"{label} Chebyshev c{i+1}:  {extras[i]:.3f}")
        elif kernel == "moffat" and len(extras) >= 3:
            gamma = np.log1p(np.exp(extras[1])) + 1e-6
            print(f"{label} moffat amp:   {extras[0]:.2f} counts")
            print(f"{label} moffat gamma: {gamma:.2f} pixels")
            print(f"{label} moffat beta:  {extras[2]:.3f}")
        elif kernel == "parzen" and len(extras) >= 2:
            print(f"{label} parzen amp:   {extras[0]:.2f} counts (peak at center)")
            print(f"{label} parzen n:     {extras[1]:.3f}")

    def set_components(self, comp1="exponential", comp2=None, comp3=None):
        """Select the radial components of the circular model by name.

        comp1 (first component, anisotropic): one of
            'exponential', 'gaussian', 'stretched', 'lorentzian', 'lorentzian2',
            'powerlaw'.
        comp2, comp3 (optional second/third background components): None, any
            comp1 option (an isotropic radial term, e.g. 'gaussian'), or one of
            'cheb', 'cheb2', 'cheb4', 'linear', 'moffat', 'parzen'.
        """
        self.comp1_kernel = comp1
        self.comp2_kernel = comp2
        self.comp3_kernel = comp3
        # Keep the legacy attribute in sync (purely for display); resolution
        # always prefers comp1_kernel/comp2_kernel/comp3_kernel.
        parts = [comp1] + [c for c in (comp2, comp3) if c is not None]
        self.circular_kernel = "+".join(parts)
        return self

    def anisotropic_circular_intensity(self, params, order, return_components=False):
        """
        Anisotropic circular background:
            I(R, θ) = baseline + A(θ) * ( s(R; D(θ)) - s(R_edge; D(θ)) )

        The components are selected independently (see set_components):
            comp1 (anisotropic, amplitude A(θ) & scale D(θ) are Fourier series):
                one of 'exponential', 'gaussian', 'stretched', 'lorentzian',
                'lorentzian2', 'powerlaw'. Edge-zeroed so it cannot absorb the
                flat baseline.
            comp2, comp3 (optional extra backgrounds): None, any comp1 option (an
                isotropic radial term, e.g. 'gaussian'), or one of
                'cheb'/'cheb2'/'cheb4', 'linear', 'moffat', 'parzen'.

        Parameter layout:
            [ baseline,
              e,                      # ONLY when self.aspect is True (aspect ratio)
              a0_amp, a0_scale,
              (a_kc_amp, a_ks_amp, a_kc_scale, a_ks_scale)*order,
              <comp1 shape extras>,   # [beta] stretched / [n] powerlaw, else none
              <comp2 extras>,
              <comp3 extras> ]

        If self.aspect is True the radius is elliptical, R = sqrt((x/e)^2 + y^2),
        so the whole background is a horizontally-stretched oval (e>1) instead of
        a circle. e=1 recovers the strictly circular model. The single extra `e`
        parameter sits right after baseline; it is omitted entirely when aspect
        is off.

        comp2 extras by kernel:
            None              : (no parameters)
            base radial       : [amp2, scale2_raw] (+ [beta] stretched / [n] powerlaw)
            'linear'          : [slope]              (slope*(R_edge-R))
            'cheb'/'cheb2'/'cheb4' : [c1..cD]        (Chebyshev poly in R, orders 1..D)
            'moffat'          : [amp2, gamma_raw, beta]  (amp2*[1+(R/gamma)^2]^-beta, edge-zeroed)
            'parzen'          : [amp2, n]            (amp2*(1-(R/R_edge)^2)^n, =0 at edge)

        order = number of Fourier harmonics (K). K=0 => isotropic.
        """
        baseline = max(params[0], 0.0)
        idx = 1
        # Radial dead-zone: all kernels sit at their plateau (s=1) inside rmin and
        # only begin to decay beyond it. The beam center is masked out to ~rmin,
        # so this keeps the fit from inventing structure in the unobserved core.
        rmin = max(params[idx], 0.0)
        idx += 1
        # Optional aspect-ratio parameter (only present when self.aspect).
        use_aspect = getattr(self, "aspect", False)
        if use_aspect:
            e = max(params[idx], 1e-3)
            idx += 1
        else:
            e = 1.0
        a0_amp = params[idx]
        idx += 1
        a0_scl = params[idx]
        idx += 1

        # Extract harmonic blocks
        cos_amp = []
        sin_amp = []
        cos_scl = []
        sin_scl = []
        for k in range(order):
            cos_amp.append(params[idx])
            idx += 1
            sin_amp.append(params[idx])
            idx += 1
            cos_scl.append(params[idx])
            idx += 1
            sin_scl.append(params[idx])
            idx += 1

        cos_amp = np.asarray(cos_amp)
        sin_amp = np.asarray(sin_amp)
        cos_scl = np.asarray(cos_scl)
        sin_scl = np.asarray(sin_scl)

        theta = self.THETA  # radians
        # Amplitude (can be signed; clamp later)
        A_theta = self._fourier_series(theta, a0_amp, cos_amp, sin_amp)
        # Raw scale -> strictly positive length scale via softplus
        scale_raw = self._fourier_series(theta, a0_scl, cos_scl, sin_scl)
        D_theta = self._softplus(scale_raw)
        A_theta = np.maximum(A_theta, 0.0)

        # Resolve the user-selected components (comp2/comp3 may be None).
        comp1_kernel, comp2_kernel, comp3_kernel = self._resolve_components()

        # Elliptical radius when aspect is on (e>1 => stretched along x), else
        # the precomputed isotropic radius. e=1 makes the two identical.
        if use_aspect:
            R = np.sqrt((self.X_centered / e) ** 2 + self.Y_centered**2)
        else:
            R = self.R
        # Offset the radius so decay starts at rmin (s(R<=rmin)=1). R_edge follows
        # the same shifted scale so the edge-zeroing of every kernel still holds.
        R = np.maximum(R - rmin, 0.0)
        R_edge = float(R.max())

        # Flat constant offset, emitted as its own component so the floor the
        # tails converge to (baseline above 0) is visible in the decomposition.
        comp_base = np.full(np.shape(R), baseline, dtype=float)

        # --- Component 1: anisotropic radial kernel. Its amplitude A(theta) and
        # length scale D(theta) are the Fourier series above. A trailing shape
        # parameter is consumed for 'stretched' (beta) and 'powerlaw' (n).
        n1_extra = self._radial_extra_count(comp1_kernel)
        c1_extra = params[idx : idx + n1_extra]
        idx += n1_extra
        beta1 = (
            c1_extra[0] if (comp1_kernel == "stretched" and len(c1_extra) >= 1) else 1.0
        )
        n_pl1 = (
            c1_extra[0] if (comp1_kernel == "powerlaw" and len(c1_extra) >= 1) else 4.0
        )
        s1 = self._radial_shape(R, D_theta, comp1_kernel, beta=beta1, n=n_pl1)
        s1_edge = self._radial_shape(R_edge, D_theta, comp1_kernel, beta=beta1, n=n_pl1)
        comp1 = A_theta * (s1 - s1_edge)

        # --- Components 2 & 3: optional extra backgrounds. None => skipped.
        # Each consumes a contiguous parameter slice sized by its kernel.
        comps = [comp1]
        I = baseline + comp1
        if comp2_kernel is not None:
            n2 = self._comp2_param_count(comp2_kernel)
            comp2 = self._build_comp2(comp2_kernel, params[idx : idx + n2], R, R_edge)
            idx += n2
            I = I + comp2
            comps.append(comp2)
        if comp3_kernel is not None:
            comp3 = self._build_comp2(comp3_kernel, params[idx:], R, R_edge)
            I = I + comp3
            comps.append(comp3)

        comps.append(comp_base)
        if return_components:
            return I, comps
        return I

    def compute_intensity(self, params, return_components=False):
        """
        Compute total intensity according to self.model.
        """
        model = getattr(self, "model", "elliptical")
        if model == "elliptical":
            print("Using elliptical model")
            # print("Parameters:", params)
            # self.print_parameters(params)
            I = self.elliptical_intensity(params, return_components=return_components)
            return I

        elif model == "circular_exponential":
            # New anisotropic circular model
            order = getattr(self, "circular_order", 0)
            return self.anisotropic_circular_intensity(
                params, order, return_components=return_components
            )

        elif model == "elliptical+circular_exponential":
            n_peaks = getattr(self, "n_peaks", 1)
            base_len = 2 + 6 * n_peaks
            params_ell = params[:base_len]
            params_circ = params[base_len:]
            order = getattr(self, "circular_order", 0)

            if return_components:
                I_ell, comps_ell = self.elliptical_intensity(
                    params_ell, return_components=True
                )
                I_circ, comps_circ = self.anisotropic_circular_intensity(
                    params_circ, order, return_components=True
                )
                return I_ell + I_circ, comps_ell + comps_circ
            else:
                I_ell = self.elliptical_intensity(params_ell, return_components=False)
                I_circ = self.anisotropic_circular_intensity(
                    params_circ, order, return_components=False
                )
                return I_ell + I_circ
        else:
            raise ValueError(f"Unknown model '{model}'")

    def residuals(self, params, neg_weight=1.0, pos_weight=1.0):
        calculated = self.compute_intensity(params, return_components=False)
        valid = np.isfinite(self.pattern) & np.isfinite(calculated)
        if self.mask is not None:
            valid &= self.mask

        if getattr(self, "log_space", False):
            # Log-space residuals: minimize sum of (log data - log model)^2.
            # Weights low-intensity regions (edges/baseline) equally with the
            # peak, preventing the optimizer from collapsing baseline to 0.
            # Requires both data and model to be strictly positive; pixels
            # where either is ≤ 0 are excluded.
            valid &= (self.pattern > 0) & (calculated > 0)
            residual = np.zeros_like(self.pattern, dtype=float)
            residual[valid] = np.log(self.pattern[valid]) - np.log(calculated[valid])
        else:
            residual = self.pattern - calculated
            residual[~valid] = 0
            residual[residual < 0] *= neg_weight
            residual[residual >= 0] *= pos_weight

        return residual.ravel()

    def fit(
        self,
        initial_params=None,
        n_peaks=1,
        K=0,
        neg_weight=1.0,
        pos_weight=1.0,
        max_nfev=200,
        loss="linear",
        model="elliptical",
        kernel="exponential",
        comp1=None,
        comp2=None,
        comp3=None,
        log_space=False,
        aspect=None,
    ):
        """
        Fit the pattern using least-squares optimization.

        model: 'elliptical', 'circular_exponential', or 'elliptical+circular_exponential'

        For the 'circular_exponential' model the radial components can be
        chosen independently:
            comp1: first (anisotropic) component. One of 'exponential' (default),
                'gaussian', 'stretched', 'lorentzian', 'lorentzian2', 'powerlaw'.
            comp2, comp3: optional second/third components. None (skip), any
                comp1 option (an isotropic radial term, e.g. 'gaussian'), or one
                of 'cheb', 'cheb2', 'cheb4', 'linear', 'moffat', 'parzen'.
        If comp1 is None, the legacy single `kernel` string is used instead
        (e.g. 'exp_cheb', 'double_exp', 'exp_moffat').

        aspect: None (keep the instance setting), True, or False. When True the
            circular model uses an elliptical radius sqrt((x/e)^2 + y^2), adding
            one aspect-ratio parameter `e` so the background can be a slightly
            stretched oval instead of a perfect circle. e=1 == circular.
        """
        self.model = model
        self.circular_order = K  # harmonic order
        self.n_peaks = n_peaks
        self.log_space = log_space
        # aspect=None keeps whatever was set on the instance (constructor or a
        # prior fit); pass aspect=True/False here to override explicitly.
        if aspect is not None:
            self.aspect = aspect
        if comp1 is not None:
            self.set_components(comp1, comp2, comp3)
        else:
            # Legacy single-string selection (decoded by _resolve_components).
            self.comp1_kernel = None
            self.comp2_kernel = None
            self.comp3_kernel = None
            self.circular_kernel = kernel

        if initial_params is None:
            initial_params = self.get_initial_params(
                n_peaks=n_peaks, K=K, model=model, verbose=True
            )
        bounds = self.get_parameter_bounds(n_peaks=n_peaks, K=K, model=model)
        initial_params = np.asarray(initial_params, dtype=float)
        for i in range(len(initial_params)):
            print(
                f"Parameter {i}: initial = {initial_params[i]:.3f}, bounds = [{bounds[0][i]:.3f}, {bounds[1][i]:.3f}]"
            )
            if initial_params[i] < bounds[0][i] or initial_params[i] > bounds[1][i]:
                print(
                    f"Warning: initial parameter {i} = {initial_params[i]:.3f} out of bounds [{bounds[0][i]:.3f}, {bounds[1][i]:.3f}]"
                )
        clipped_params = np.clip(initial_params, bounds[0], bounds[1])
        if not np.allclose(initial_params, clipped_params):
            print("Clipping initial parameters to optimizer bounds before fitting.")
            initial_params = clipped_params

        result = least_squares(
            lambda p: self.residuals(p, neg_weight=neg_weight, pos_weight=pos_weight),
            initial_params,
            bounds=bounds,
            verbose=1,
            max_nfev=max_nfev,
            ftol=1e-8,
            xtol=1e-8,
            gtol=1e-8,
            loss=loss,
        )

        self.fitted_params = result.x
        if self.downsample_factor > 1:
            self.fitted_params = self._scale_params_to_original_resolution(
                self.fitted_params
            )

        print("\nFit completed!")
        print(f"Final cost: {result.cost:.2e}")
        print(f"Success: {result.success}")
        return result

    def _scale_params_to_original_resolution(self, params):
        """
        Scale position- and width-related parameters back to the original resolution.
        """
        scaled_params = params.copy()
        scaled_params[0] *= self.downsample_factor  # A (ellipticity)
        n_peaks = (len(params) - 2) // 6
        for i in range(n_peaks):
            idx = 2 + i * 6
            scaled_params[idx + 1] *= self.downsample_factor  # u0
            scaled_params[idx + 2] *= self.downsample_factor  # uw
            scaled_params[idx + 3] *= 1.0  # v0 (degrees, no scaling needed)
            scaled_params[idx + 4] *= 1.0  # vw (degrees, no scaling needed)
        return scaled_params

    def get_initial_params(self, n_peaks=1, K=1, model="elliptical", verbose=False):
        """
        For circular model: n_peaks = harmonic order K.
        """
        params = []

        # Common helpers
        finite = np.isfinite(self.pattern)
        baseline_guess = np.percentile(self.pattern[finite], 1)
        max_val = np.percentile(self.pattern[finite], 95)

        if model == "elliptical":
            # A (semi-focal distance)
            A0 = 1.0
            params.append(A0)
            # Baseline
            params.append(baseline_guess)

            # Estimate uw and set vw guesses
            u_tmp, _ = self.cartesian_to_elliptic_np(
                self.X_centered, self.Y_centered, A0, degrees=True, normalize=True
            )
            u_span = np.nanpercentile(u_tmp, 95) - np.nanpercentile(u_tmp, 5)
            uw_guess = max(u_span * 0.08, 0.05)
            vw_guess = 15.0

            # Peak 1 (equator)
            params.extend(
                [
                    1.0,  # amp
                    15,  # 15.0,             # u0
                    250.0,  # uw
                    2.0,  # v0 (deg)
                    10.0,  # vw (deg)
                    2,  # m (modified Lorentzian parameter)
                ]
            )

            # Optional extra peaks (kept same as before)
            if n_peaks >= 2:
                params.extend(
                    [
                        400.0,
                        -20.0,  # np.nanpercentile(u_tmp, 60),
                        20.0,
                        11.0,
                        30.0,
                        2.0,
                    ]
                )
            if n_peaks >= 3:
                params.extend(
                    [
                        max_val * 200,
                        0.0,  # np.nanpercentile(u_tmp, 30),
                        uw_guess * 2.0,
                        0.0,
                        vw_guess,
                        2,
                    ]
                )
            if verbose:
                print(f"Initial parameters (elliptical, {n_peaks} peak(s)): {params}")
            return np.array(params)

        elif model == "circular_exponential":
            K = K
            finite = np.isfinite(self.pattern)
            baseline_guess = np.percentile(self.pattern[finite], 1)
            max_val = np.percentile(self.pattern[finite], 95)
            amp0 = max_val - baseline_guess
            # Characteristic decay ~ median radius
            d0 = np.median(self.R[np.isfinite(self.R)])
            r_max = float(self.R.max())
            d_raw_hi = min(0.5 * r_max, 1e3)  # must match get_parameter_bounds
            d_raw = (
                np.log(np.expm1(d0)) if d0 > 1 else d0
            )  # raw param s.t. softplus(d_raw)=d0
            d_raw = min(d_raw, d_raw_hi - 1e-3)  # ensure feasible
            params = [baseline_guess, 20.0]  # baseline, rmin (radial dead-zone)
            if getattr(self, "aspect", False):
                params.append(1.0)  # e: start circular (e=1)
            params += [amp0, d_raw]  # store raw a0_decay pre-softplus
            # Add zeroed harmonic coefficients
            for _ in range(K):
                params.extend(
                    [0.0, 0.0, 0.0, 0.0]
                )  # a_kc_amp, a_ks_amp, a_kc_dec, a_ks_dec

            # Append component-specific extra parameters.
            comp1_kernel, comp2_kernel, comp3_kernel = self._resolve_components()

            # comp1 shape parameter (beta/n) if its kernel needs one.
            if comp1_kernel == "stretched":
                params.append(1.0)  # beta (1.0 == plain exponential start)
            elif comp1_kernel == "powerlaw":
                params.append(4.0)  # n (Porod-like default)

            # comp2 then comp3 parameters (same layout, see _default_comp_params).
            params.extend(
                self._default_comp_params(comp2_kernel, amp0, d0, r_max, d_raw_hi)
            )
            params.extend(
                self._default_comp_params(comp3_kernel, amp0, d0, r_max, d_raw_hi)
            )

            if verbose:
                print(
                    f"Initial parameters (circular comp1={comp1_kernel}, "
                    f"comp2={comp2_kernel}, comp3={comp3_kernel}, K={K}): {params}"
                )
            return np.array(params)

        elif model == "elliptical+circular_exponential":
            params_ell = list(
                self.get_initial_params(
                    n_peaks=n_peaks, K=K, model="elliptical", verbose=False
                )
            )
            params_circ = list(
                self.get_initial_params(
                    n_peaks=n_peaks, K=K, model="circular_exponential", verbose=False
                )
            )
            params = params_ell + params_circ
            if verbose:
                print(
                    f"Initial parameters (elliptical+circular_exponential, {n_peaks} peak(s), K={K}): {params}"
                )
            return np.array(params)

        else:
            raise ValueError(f"Unknown model '{model}'")

    def get_parameter_bounds(self, n_peaks=1, K=1, model="elliptical"):
        """Generate parameter bounds for optimization for the selected model."""

        if model == "elliptical":
            lower = [0, -1]
            upper = [500, np.inf]
            for _ in range(n_peaks):
                lower.extend([0, -500, 1e-6, 0, 1e-6, 0.0])  # amp, u0, uw, v0, vw, m
                upper.extend([np.inf, 500, 500, 180, 180, 5.0])
            return (lower, upper)

        elif model == "circular_exponential":
            K = getattr(self, "circular_order", 0)
            r_max = float(self.R.max())
            # Bound raw decay so effective D ≲ ~0.5*r_max
            d_raw_lo = -20.0
            d_raw_hi = min(0.5 * r_max, 1e3)
            lower = [0.0, 0.0]  # baseline, rmin
            upper = [np.inf, 35.0]
            if getattr(self, "aspect", False):
                lower += [0.2]
                upper += [5.0]  # e: aspect ratio (~oval)
            lower += [0.0, d_raw_lo] + [-1e2] * (4 * K)
            upper += [np.inf, d_raw_hi] + [1e2] * (4 * K)

            # Component-specific extra parameter bounds
            comp1_kernel, comp2_kernel, comp3_kernel = self._resolve_components()

            # comp1 shape parameter (beta/n).
            if comp1_kernel == "stretched":
                lower += [0.1]
                upper += [3.0]  # beta
            elif comp1_kernel == "powerlaw":
                lower += [0.5]
                upper += [8.0]  # n

            # comp2 then comp3 bounds (same layout, see _comp_param_bounds).
            for comp_kernel in (comp2_kernel, comp3_kernel):
                lo, hi = self._comp_param_bounds(comp_kernel, d_raw_lo, d_raw_hi)
                lower += lo
                upper += hi
            return (lower, upper)

        elif model == "elliptical+circular_exponential":
            lower_ell, upper_ell = self.get_parameter_bounds(
                n_peaks=n_peaks, K=K, model="elliptical"
            )
            lower_circ, upper_circ = self.get_parameter_bounds(
                n_peaks=n_peaks, K=K, model="circular_exponential"
            )
            return (lower_ell + lower_circ, upper_ell + upper_circ)
        else:
            raise ValueError(f"Unknown model '{model}'")

    def plot_results(self, figsize=(15, 5)):
        """Plot original, fitted, and residual patterns."""
        if not hasattr(self, "fitted_params"):
            raise ValueError("Must run fit() before plotting results")

        fitted = self.compute_intensity(self.fitted_params)
        residual = self.pattern - fitted

        fig, axes = plt.subplots(1, 3, figsize=figsize)

        # Use percentiles for better visualization
        vmin = np.percentile(self.pattern[np.isfinite(self.pattern)], 1)
        vmax = np.percentile(self.pattern[np.isfinite(self.pattern)], 99)

        # Original
        im0 = axes[0].imshow(
            self.pattern, origin="lower", vmin=vmin, vmax=vmax, cmap="viridis"
        )
        axes[0].set_title("Original Pattern")
        axes[0].set_xlabel("X (pixels)")
        axes[0].set_ylabel("Y (pixels)")
        axes[0].axhline(self.y0, color="r", linestyle="--", alpha=0.3)
        axes[0].axvline(self.x0, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im0, ax=axes[0])

        # Fitted
        im1 = axes[1].imshow(
            fitted, origin="lower", vmin=vmin, vmax=vmax, cmap="viridis"
        )
        axes[1].set_title("Fitted Pattern")
        axes[1].set_xlabel("X (pixels)")
        axes[1].axhline(self.y0, color="r", linestyle="--", alpha=0.3)
        axes[1].axvline(self.x0, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im1, ax=axes[1])

        # Residual
        res_max = np.percentile(np.abs(residual[np.isfinite(residual)]), 99)
        im2 = axes[2].imshow(
            residual, origin="lower", cmap="RdBu_r", vmin=-res_max, vmax=res_max
        )
        axes[2].set_title("Residual")
        axes[2].set_xlabel("X (pixels)")
        axes[2].axhline(self.y0, color="k", linestyle="--", alpha=0.3)
        axes[2].axvline(self.x0, color="k", linestyle="--", alpha=0.3)
        plt.colorbar(im2, ax=axes[2])

        plt.tight_layout()
        return fig

    def plot_components(self, figsize=None):
        """Plot individual peak components."""
        if not hasattr(self, "fitted_params"):
            raise ValueError("Must run fit() before plotting components")

        fitted, components = self.compute_intensity(
            self.fitted_params, return_components=True
        )

        n_peaks = len(components)
        if n_peaks == 0:
            print("No components to plot")
            return None

        if figsize is None:
            figsize = (5 * n_peaks, 5)

        fig, axes = plt.subplots(1, n_peaks, figsize=figsize)

        if n_peaks == 1:
            axes = [axes]

        titles = [
            "Equatorial Streak",
            "Lamellar Reflection",
            "Fibrillar Interference",
            "Circular Exp.",
        ]
        # If model=circular, there will be only one component: 'Circular Exp.'
        # If model=both, the last component is circular.

        for i, (ax, comp) in enumerate(zip(axes, components)):
            vmax = np.percentile(comp[np.isfinite(comp)], 99)
            im = ax.imshow(comp, origin="lower", vmin=0, vmax=vmax, cmap="viridis")
            title_idx = min(i, len(titles) - 1)
            # If circular-only, rename explicitly
            if self.model == "circular":
                ax.set_title("Circular Exp.")
            else:
                ax.set_title(f"Component {i+1}")
            ax.set_xlabel("X (pixels)")
            if i == 0:
                ax.set_ylabel("Y (pixels)")
            ax.axhline(self.y0, color="r", linestyle="--", alpha=0.3)
            ax.axvline(self.x0, color="r", linestyle="--", alpha=0.3)
            plt.colorbar(im, ax=ax)

        plt.tight_layout()
        return fig

    def print_parameters(self, params=None):
        """Print fitted parameters in a readable format."""

        if params is None:
            params = self.fitted_params
        model = getattr(self, "model", "elliptical")
        print("\n" + "=" * 60)
        print(f"FITTED PARAMETERS (model: {model})")
        print("=" * 60)

        if model == "elliptical":
            print(f"Ellipticity (A):  {params[0]:.2f} pixels")
            print(f"Baseline:         {params[1]:.2f} counts")
            print()
            n_peaks = (len(params) - 2) // 6
            peak_names = [
                "Equatorial Streak",
                "Lamellar Reflection",
                "Fibrillar Interference",
            ]
            for i in range(n_peaks):
                idx = 2 + i * 6
                print(
                    f"Peak {i+1}: {peak_names[i] if i < len(peak_names) else 'Unknown'}"
                )
                print(f"  Amplitude:    {params[idx]:.2f} counts")
                print(f"  u-position:   {params[idx+1]:.2f} pixels")
                print(f"  u-width:      {params[idx+2]:.2f} pixels (FWHM)")
                print(f"  v-position:   {params[idx+3]:.2f} degrees")
                print(f"  v-width:      {params[idx+4]:.2f} degrees (FWHM)")
                print(f"  m:            {params[idx+5]:.2f}")
                print()
            print("=" * 60)
            return

        if model == "circular_exponential":
            comp1_kernel, comp2_kernel, comp3_kernel = self._resolve_components()
            K = getattr(self, "circular_order", 0)
            print(f"Comp1 kernel:     {comp1_kernel}")
            print(f"Comp2 kernel:     {comp2_kernel}")
            print(f"Comp3 kernel:     {comp3_kernel}")
            print(f"Baseline:         {params[0]:.2f} counts")
            print(f"Radial rmin:      {params[1]:.2f} pixels (decay starts here)")
            print(f"Harmonic order K: {K}")

            # Split the trailing extras: comp1 shape param, then comp2, then comp3.
            idx = 4 + 4 * K
            n1 = self._radial_extra_count(comp1_kernel)
            c1_extra = params[idx : idx + n1]
            rest = params[idx + n1 :]
            self._print_comp_params(
                comp1_kernel, [params[2], params[3]] + list(c1_extra), label="Comp1"
            )

            n2 = self._comp2_param_count(comp2_kernel)
            self._print_comp_params(comp2_kernel, rest[:n2], label="Comp2")
            self._print_comp_params(comp3_kernel, rest[n2:], label="Comp3")
            print("=" * 60)
            return

        if model == "elliptical+circular_exponential":
            n_peaks = getattr(self, "n_peaks", 1)
            base_len = 2 + 6 * n_peaks
            params_ell = params[:base_len]
            params_circ = params[base_len:]
            print(f"Ellipticity (A):  {params_ell[0]:.2f} pixels")
            print(f"Baseline:         {params_ell[1]:.2f} counts")
            peak_names = [
                "Equatorial Streak",
                "Lamellar Reflection",
                "Fibrillar Interference",
            ]
            for i in range(n_peaks):
                idx = 2 + i * 6
                print(
                    f"Peak {i+1}: {peak_names[i] if i < len(peak_names) else 'Unknown'}"
                )
                print(f"  Amplitude:    {params_ell[idx]:.2f} counts")
                print(f"  u-position:   {params_ell[idx+1]:.2f} pixels")
                print(f"  u-width:      {params_ell[idx+2]:.2f} pixels (FWHM)")
                print(f"  v-position:   {params_ell[idx+3]:.2f} degrees")
                print(f"  v-width:      {params_ell[idx+4]:.2f} degrees (FWHM)")
                print(f"  m:            {params_ell[idx+5]:.2f}")
                print()
            kernel = getattr(self, "circular_kernel", "exponential")
            K = getattr(self, "circular_order", 0)
            eff_D = np.log1p(np.exp(params_circ[2])) + 1e-6
            scale_name = (
                "xi"
                if kernel in ("lorentzian", "lorentzian2")
                else ("c" if kernel == "powerlaw" else "D")
            )
            print(f"Circular kernel:     {kernel}")
            print(f"Circular baseline:   {params_circ[0]:.2f} counts")
            print(f"Circular amp:        {params_circ[1]:.2f} counts")
            print(f"Circular {scale_name} (eff):  {eff_D:.2f} pixels")
            print("=" * 60)
            return

    def plot_coordinates(
        self, figsize=(15, 5), A=100, degrees=True, normalize=True, polar=False
    ):
        """Plot the elliptical coordinates (u, v) over the pattern."""
        if polar:
            u, v = self.cartesian_to_polar_np(
                self.X_centered, self.Y_centered, degrees=degrees, normalize=normalize
            )
            title = "Polar Coordinate System"
        else:
            u, v = self.cartesian_to_elliptic_np(
                self.X_centered,
                self.Y_centered,
                A,
                degrees=degrees,
                normalize=normalize,
            )
            title = "Elliptical Coordinate System (A={})".format(A)

        fig, axes = plt.subplots(1, 2, figsize=figsize)

        im0 = axes[0].imshow(u, origin="lower", cmap="viridis")
        axes[0].set_title(f"{title} u.")
        axes[0].set_xlabel("X (pixels)")
        axes[0].set_ylabel("Y (pixels)")
        axes[0].axhline(self.y0, color="r", linestyle="--", alpha=0.3)
        axes[0].axvline(self.x0, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im0, ax=axes[0])

        im1 = axes[1].imshow(v, origin="lower", cmap="viridis")
        axes[1].set_title(f"{title} v (degrees)")
        axes[1].set_xlabel("X (pixels)")
        axes[1].axhline(self.y0, color="r", linestyle="--", alpha=0.3)
        axes[1].axvline(self.x0, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im1, ax=axes[1])

    def plot_elliptical_coordinate_system(self, A=100, figsize=(12, 5), clip=[0, 500]):
        """
        Plot the SAXS pattern in elliptical coordinate system with u, v as axes.

        Parameters:
        -----------
        A : float
            Elliptical parameter for coordinate transformation
        figsize : tuple
            Figure size (width, height)
        """
        from scipy.interpolate import griddata

        # Get elliptical coordinates for all points
        u, v = self.cartesian_to_elliptic_np(
            self.X_centered, self.Y_centered, A, degrees=True, normalize=True
        )

        # Flatten arrays for interpolation
        u_flat = u.flatten()
        v_flat = v.flatten()
        pattern_flat = self.pattern.flatten()

        # Remove invalid points
        valid_mask = (
            np.isfinite(pattern_flat) & np.isfinite(u_flat) & np.isfinite(v_flat)
        )
        u_valid = u_flat[valid_mask]
        v_valid = v_flat[valid_mask]
        pattern_valid = pattern_flat[valid_mask]

        # Create regular grid in elliptical coordinates
        u_min, u_max = u_valid.min(), u_valid.max()
        v_min, v_max = 0, 360  # Full angular range

        # Create grid points
        n_u, n_v = 200, 360  # Resolution in u and v
        u_grid = np.linspace(u_min, u_max, n_u)
        v_grid = np.linspace(v_min, v_max, n_v)
        U_grid, V_grid = np.meshgrid(u_grid, v_grid)

        # Interpolate intensity values onto the regular elliptical grid
        print("Interpolating intensity values...")
        pattern_elliptical = griddata(
            (u_valid, v_valid),
            pattern_valid,
            (U_grid, V_grid),
            method="linear",
            fill_value=0,
        )

        # Create plots
        fig, axes = plt.subplots(1, 2, figsize=figsize)

        # Original pattern in Cartesian coordinates
        # vmin = np.percentile(self.pattern[np.isfinite(self.pattern)], 1)
        # vmax = np.percentile(self.pattern[np.isfinite(self.pattern)], 99)

        im0 = axes[0].imshow(
            np.clip(self.pattern, clip[0], clip[1]),
            origin="lower",
            cmap="viridis",
            # ,extent=[0, self.nx, 0, self.ny]
        )
        axes[0].set_title("Original Pattern (Cartesian)")
        axes[0].set_xlabel("X (pixels)")
        axes[0].set_ylabel("Y (pixels)")
        axes[0].axhline(self.y0, color="r", linestyle="--", alpha=0.5)
        axes[0].axvline(self.x0, color="r", linestyle="--", alpha=0.5)
        plt.colorbar(im0, ax=axes[0], label="Intensity")

        # Pattern in elliptical coordinates
        im1 = axes[1].imshow(
            np.clip(pattern_elliptical, clip[0], clip[1]),
            origin="lower",
            cmap="viridis",
            # extent=[u_min, u_max, v_min, v_max],
            aspect="auto",
        )
        axes[1].set_title("Pattern in Elliptical Coordinates")
        axes[1].set_xlabel("u (elliptical radial)")
        axes[1].set_ylabel("v (degrees)")
        axes[1].set_ylim(0, 360)
        # Add horizontal lines at key angles
        for angle in [0, 90, 180, 270]:
            axes[1].axhline(angle, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im1, ax=axes[1], label="Intensity")

        plt.tight_layout()
        return fig, pattern_elliptical, U_grid, V_grid

    # plot polar coordinate system
    def plot_polar_coordinate_system(self, figsize=(6, 5), clip=[0, 500]):
        """
        Plot the SAXS pattern in polar coordinate system with r, theta as axes.

        Parameters:
        -----------
        figsize : tuple
            Figure size (width, height)
        """
        from scipy.interpolate import griddata

        # Get polar coordinates for all points
        r, theta = self.cartesian_to_polar_np(
            self.X_centered, self.Y_centered, degrees=True, normalize=True
        )

        # Flatten arrays for interpolation
        r_flat = r.flatten()
        theta_flat = theta.flatten()
        pattern_flat = self.pattern.flatten()

        # Remove invalid points
        valid_mask = (
            np.isfinite(pattern_flat) & np.isfinite(r_flat) & np.isfinite(theta_flat)
        )
        r_valid = r_flat[valid_mask]
        theta_valid = theta_flat[valid_mask]
        pattern_valid = pattern_flat[valid_mask]

        # Create regular grid in polar coordinates
        r_min, r_max = r_valid.min(), r_valid.max()
        theta_min, theta_max = 0, 360  # Full angular range

        # Create grid points
        n_r, n_theta = 200, 360  # Resolution in r and theta
        r_grid = np.linspace(r_min, r_max, n_r)
        theta_grid = np.linspace(theta_min, theta_max, n_theta)
        R_grid, Theta_grid = np.meshgrid(r_grid, theta_grid)

        # Interpolate intensity values onto the regular polar grid
        print("Interpolating intensity values...")
        pattern_polar = griddata(
            (r_valid, theta_valid),
            pattern_valid,
            (R_grid, Theta_grid),
            method="linear",
            fill_value=0,
        )

        # Create plots
        fig, ax = plt.subplots(figsize=figsize)

        im = ax.imshow(
            np.clip(pattern_polar, clip[0], clip[1]),
            origin="lower",
            cmap="viridis",
            extent=[r_min, r_max, theta_min, theta_max],
            aspect="auto",
        )
        ax.set_title("Pattern in Polar Coordinates")
        ax.set_xlabel("r (radial distance)")
        ax.set_ylabel("theta (degrees)")
        ax.set_ylim(0, 360)
        # Add horizontal lines at key angles
        for angle in [0, 90, 180, 270]:
            ax.axhline(angle, color="r", linestyle="--", alpha=0.3)
        plt.colorbar(im, ax=ax, label="Intensity")

        plt.tight_layout()
        return fig, pattern_polar, R_grid, Theta_grid

    def plot_radial_azimuthal_profiles(self, A=100, figsize=(15, 5)):
        """
        Plot radial and azimuthal profiles in elliptical coordinates.

        Parameters:
        -----------
        A : float
            Elliptical parameter for coordinate transformation
        figsize : tuple
            Figure size (width, height)
        """
        # Get elliptical coordinates
        u, v = self.cartesian_to_elliptic_np(
            self.X_centered, self.Y_centered, A, degrees=True, normalize=True
        )

        fig, axes = plt.subplots(1, 3, figsize=figsize)

        # 1. Radial profile (averaged over all angles)
        u_flat = u.flatten()
        pattern_flat = self.pattern.flatten()
        valid_mask = np.isfinite(pattern_flat) & np.isfinite(u_flat)

        u_bins = np.linspace(0, u_flat[valid_mask].max(), 100)
        u_centers = (u_bins[:-1] + u_bins[1:]) / 2

        radial_profile = []
        for i in range(len(u_bins) - 1):
            mask = (u_flat >= u_bins[i]) & (u_flat < u_bins[i + 1]) & valid_mask
            if np.sum(mask) > 0:
                radial_profile.append(np.mean(pattern_flat[mask]))
            else:
                radial_profile.append(0)

        axes[0].plot(u_centers, radial_profile, "b-", linewidth=2)
        axes[0].set_xlabel("u (elliptical radial)")
        axes[0].set_ylabel("Average Intensity")
        axes[0].set_title("Radial Profile")
        axes[0].grid(True, alpha=0.3)

        # 2. Azimuthal profile (averaged over all radii)
        v_flat = v.flatten()
        v_bins = np.linspace(0, 360, 180)
        v_centers = (v_bins[:-1] + v_bins[1:]) / 2

        azimuthal_profile = []
        for i in range(len(v_bins) - 1):
            mask = (v_flat >= v_bins[i]) & (v_flat < v_bins[i + 1]) & valid_mask
            if np.sum(mask) > 0:
                azimuthal_profile.append(np.mean(pattern_flat[mask]))
            else:
                azimuthal_profile.append(0)

        axes[1].plot(v_centers, azimuthal_profile, "r-", linewidth=2)
        axes[1].set_xlabel("v (degrees)")
        axes[1].set_ylabel("Average Intensity")
        axes[1].set_title("Azimuthal Profile")
        axes[1].set_xlim(0, 360)
        axes[1].grid(True, alpha=0.3)
        # Mark key angles
        for angle in [0, 90, 180, 270]:
            axes[1].axvline(angle, color="gray", linestyle="--", alpha=0.5)

        # 3. 2D histogram in elliptical coordinates
        H, u_edges, v_edges = np.histogram2d(
            u_flat[valid_mask],
            v_flat[valid_mask],
            bins=[50, 180],
            weights=pattern_flat[valid_mask],
        )

        # Normalize by counts to get average intensity
        counts, _, _ = np.histogram2d(
            u_flat[valid_mask], v_flat[valid_mask], bins=[50, 180]
        )
        with np.errstate(divide="ignore", invalid="ignore"):
            H_avg = np.divide(H, counts, out=np.zeros_like(H), where=counts != 0)

        im = axes[2].imshow(
            H_avg.T,
            origin="lower",
            cmap="viridis",
            extent=[u_edges[0], u_edges[-1], v_edges[0], v_edges[-1]],
            aspect="auto",
        )
        axes[2].set_xlabel("u (elliptical radial)")
        axes[2].set_ylabel("v (degrees)")
        axes[2].set_title("2D Histogram (Elliptical)")
        plt.colorbar(im, ax=axes[2], label="Average Intensity")

        plt.tight_layout()
        return fig


# Example usage
if __name__ == "__main__":
    # Generate synthetic SAXS pattern for testing
    print("Generating synthetic SAXS pattern...")
    np.random.seed(42)
    sizex = 300
    sizey = 100

    # Create coordinate grids
    y_center = sizey // 2
    x_center = sizex // 2
    x = np.arange(sizex)
    y = np.arange(sizey)
    X, Y = np.meshgrid(x, y)

    # Initialize pattern
    pattern = np.zeros((sizey, sizex))

    # Add equatorial streak (strong horizontal feature)
    streak_intensity = 100
    streak_y_width = 30
    streak_x_width = 100
    streak = (
        streak_intensity
        * np.exp(-((Y - y_center) ** 2) / (2 * streak_y_width**2))
        * np.exp(-((X - x_center) ** 2) / (2 * streak_x_width**2))
    )
    pattern += streak

    # Add some background
    pattern += 1

    # Add Poisson noise
    pattern = np.array(np.random.poisson(pattern), dtype=float)

    # Add Gaussian noise
    noise_level = 1.0
    pattern += np.random.normal(0, noise_level, pattern.shape)

    # normalize pattern
    pattern /= np.max(pattern)

    print(f"Pattern shape: {pattern.shape}")
    print(f"Pattern range: [{pattern.min():.1f}, {pattern.max():.1f}]")
    print(f"Pattern mean: {pattern.mean():.1f}")

    # Initialize fitter
    fitter = EllipticalSAXSFitter(pattern)

    # Fit with 1 peak (equatorial streak only) - same as before
    result = fitter.fit(n_peaks=1, model="elliptical")
    fitted_params = result.x
    # fitter.print_parameters()

    print("\nGenerating plots...")
    fitter.plot_results()
    A = fitted_params[0]
    fitter.plot_coordinates(A=A, degrees=False, normalize=False)

    # Plot in elliptical coordinate system
    fitter.plot_elliptical_coordinate_system(A=A)
    fitter.plot_radial_azimuthal_profiles(A=A)

    plt.show()
