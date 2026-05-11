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

import numpy as np
from matplotlib.patches import Circle

from .interaction_tool import InteractionTool


_PREVIEW_LABEL = 'radius_preview'


class RadiusTool(InteractionTool):
    """
    Interactive tool: pick a radius by clicking on the image.

    Workflow:
        1. Tool draws a dashed preview circle centred on ``get_center_func()``
           passing through the cursor as the user moves.
        2. On mouse release the radius (rounded to int pixels) is captured
           and ``completed`` is set to ``True`` so the host can pick it up
           via ``toolCompleted``.

    The result returned by :meth:`get_result` is an int pixel radius.
    """

    def __init__(self, axes, canvas, get_center_func, color='yellow'):
        """
        Args:
            axes: matplotlib Axes
            canvas: matplotlib canvas
            get_center_func: callable returning the current center in image
                coordinates ``(x, y)``, or ``None`` if no image is loaded.
            color: preview-circle colour (matplotlib spec).
        """
        super().__init__(axes, canvas)
        self.get_center_func = get_center_func
        self.color = color
        self.radius = None
        self.completed = False

    def _on_activate(self):
        self.radius = None
        self.completed = False
        # Only remove our own preview artists (don't touch image / overlays).
        self.remove_labeled_items([_PREVIEW_LABEL])

    def _on_deactivate(self):
        self.remove_labeled_items([_PREVIEW_LABEL])

    def handle_click(self, event) -> bool:
        # We act on release; capture press to mark event handled and prevent
        # default canvas behaviour (pan, etc.) while the tool is active.
        return event.inaxes == self.axes

    def handle_motion(self, event) -> bool:
        if not self.is_active or event.inaxes != self.axes:
            return False
        center = self.get_center_func() if self.get_center_func else None
        if center is None or event.xdata is None or event.ydata is None:
            return False
        cx, cy = center
        r = float(np.hypot(event.xdata - cx, event.ydata - cy))

        self.remove_labeled_items([_PREVIEW_LABEL])
        self.axes.add_patch(Circle(
            (cx, cy), r,
            fill=False, ec=self.color, ls='--', lw=1.5,
            label=_PREVIEW_LABEL,
        ))
        self.canvas.draw_idle()
        return True

    def handle_release(self, event) -> bool:
        if not self.is_active or event.inaxes != self.axes:
            return False
        center = self.get_center_func() if self.get_center_func else None
        if center is None or event.xdata is None or event.ydata is None:
            return False
        cx, cy = center
        self.radius = int(round(np.hypot(event.xdata - cx, event.ydata - cy)))
        self.completed = True
        self.remove_labeled_items([_PREVIEW_LABEL])
        return True

    def get_result(self):
        return self.radius
