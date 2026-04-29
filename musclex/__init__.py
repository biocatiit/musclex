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
included in all cdcopies or substantial portions of the Software.

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

import datetime as _datetime
import os as _os
import sys as _sys
import subprocess as _subprocess

def _get_version():
    _base = '1.29.0-beta.2'
    try:
        _branch = _subprocess.check_output(
            ['git', 'rev-parse', '--abbrev-ref', 'HEAD'],
            stderr=_subprocess.DEVNULL
        ).decode().strip()
    except Exception:
        _branch = ''
    return f'{_base}(dev)' if _branch == 'dev' else _base

__version__ = _get_version()


def _is_dev_build() -> bool:
    return __version__.endswith('(dev)')


def _looks_like_test_runner() -> bool:
    """Best-effort guess of whether we're inside a unittest / pytest run.

    Test runs auto-enabling capture would silently fill the user's home
    directory with pickles every CI invocation, which is surprising. We
    keep the auto-enable strictly for interactive ``musclex pt`` / ``pth``
    style sessions.
    """
    if 'PYTEST_CURRENT_TEST' in _os.environ:
        return True
    argv0 = _os.path.basename(_sys.argv[0]) if _sys.argv else ''
    if argv0 in ('pytest', 'py.test'):
        return True
    if 'unittest' in _sys.argv:
        return True
    if any('unittest' in a for a in _sys.argv[:2]):  # `python -m unittest ...`
        return True
    return False


def _maybe_enable_dev_capture():
    """Auto-enable A/B fit capture for dev builds.

    Activation rules:

    * Only on dev builds (``__version__`` ends with ``(dev)``).
    * Skipped if running under a test runner (see ``_looks_like_test_runner``).
    * Respects any explicit ``MUSCLEX_CAPTURE_FITS`` setting (including
      ``=0`` to opt out).
    * Defaults the capture dir to ``~/.musclex/fit_captures/<YYYY-MM-DD>/``
      so pickles never land inside the git working tree.
    * Defaults the tag to ``dev`` (overridable via ``MUSCLEX_CAPTURE_TAG``).

    Prints a single one-line notice to stderr so users know capture is on
    and how to disable it. Disable with::

        export MUSCLEX_CAPTURE_FITS=0
    """
    if not _is_dev_build():
        return
    if _looks_like_test_runner():
        return
    if 'MUSCLEX_CAPTURE_FITS' in _os.environ:
        return  # respect explicit user choice (set or unset to '0')

    today = _datetime.date.today().isoformat()
    default_dir = _os.path.expanduser(_os.path.join('~', '.musclex', 'fit_captures', today))

    _os.environ['MUSCLEX_CAPTURE_FITS'] = '1'
    _os.environ.setdefault('MUSCLEX_CAPTURE_DIR', default_dir)
    _os.environ.setdefault('MUSCLEX_CAPTURE_TAG', 'dev')

    _sys.stderr.write(
        f'[musclex] dev build: A/B fit capture ENABLED -> {default_dir}\n'
        f'[musclex]   disable with: export MUSCLEX_CAPTURE_FITS=0\n'
    )


def _show_fatal_dialog(title, message):
    """Show a GUI error dialog using Tkinter (stdlib, independent of Qt).

    We deliberately avoid Qt here: the whole point of this diagnostic is
    that the Qt environment is broken, so importing PySide6 to show a
    dialog could itself crash. Tkinter ships with CPython and is completely
    independent.
    """
    if not (_os.environ.get('DISPLAY') or _os.environ.get('WAYLAND_DISPLAY')):
        return False
    try:
        import tkinter as _tk
        from tkinter import messagebox as _mb
        root = _tk.Tk()
        root.withdraw()
        _mb.showerror(title, message)
        root.destroy()
        return True
    except Exception:
        return False


def _check_pyside6_environment():
    """Detect PySide6 loaded from outside the active Python prefix.

    Mixing a pip user-site PySide6 with a conda env (or another venv) that
    sets QT_PLUGIN_PATH causes Qt core libs and the xcb platform plugin to
    come from different builds, leading to ABI-mismatch segfaults on
    keyboard input. We fail fast with a clear message instead of crashing
    later in C++.
    """
    if _os.environ.get('MUSCLEX_SKIP_ENV_CHECK'):
        return
    try:
        import importlib.util
        spec = importlib.util.find_spec('PySide6')
        if spec is None or not spec.origin:
            return
        ps6_path = _os.path.realpath(spec.origin)
        py_prefix = _os.path.realpath(_sys.prefix)
        base_prefix = _os.path.realpath(getattr(_sys, 'base_prefix', _sys.prefix))
        if ps6_path.startswith(py_prefix) or ps6_path.startswith(base_prefix):
            return
        title = 'MuscleX: PySide6 environment mismatch'
        message = (
            "PySide6 environment mismatch (will cause segfaults).\n\n"
            f"Loaded from: {ps6_path}\n"
            f"Expected in: {py_prefix}\n\n"
            "Pick one to fix:\n"
            "  1. Delete the conflicting local PySide6 (NOT the system one):\n"
            f"       rm -rf {_os.path.dirname(ps6_path)}/PySide6*\n"
            f"       rm -rf {_os.path.dirname(ps6_path)}/shiboken6*\n\n"
            "  2. Set environment variable before running musclex:\n"
            "       export PYTHONNOUSERSITE=1\n\n"
            "  3. Don't use conda — use a venv or pip install instead."
        )
        _sys.stderr.write('\n[musclex] FATAL: ' + message + '\n')
        _show_fatal_dialog(title, message)
        _sys.exit(1)
    except SystemExit:
        raise
    except Exception:
        # Never let the diagnostic itself break musclex startup.
        pass


_check_pyside6_environment()
_maybe_enable_dev_capture()
