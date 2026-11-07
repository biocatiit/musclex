import subprocess
import sys


def test_utils_package_defers_image_data_import():
    script = """
import sys
import musclex.utils

assert "musclex.utils.image_data" not in sys.modules
assert not any(name.startswith("pyFAI") for name in sys.modules)

from musclex.utils import ImageData

assert ImageData.__name__ == "ImageData"
assert "musclex.utils.image_data" in sys.modules
"""
    subprocess.run([sys.executable, "-c", script], check=True)
