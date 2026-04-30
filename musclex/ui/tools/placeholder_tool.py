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

from .interaction_tool import InteractionTool


class PlaceholderTool(InteractionTool):
    """
    A placeholder tool that blocks ImageViewerWidget's built-in left-click panning.

    This tool is used by GUIs (e.g. ProjectionTracesGUI, XRayViewerGUI) that still
    have legacy ``self.function`` state-machine interactions wired through the
    ``canvasClicked`` / ``mouseMoved`` signals on the image viewer. By occupying
    the ToolManager's active-tool slot while the legacy function is in progress,
    we get two things for free:

    1. ``ImageViewerWidget`` skips its built-in left-click pan
       (see ``_handle_pan_start`` — it bails when ``has_active_tool`` is True).
    2. Mutual exclusion with other registered tools: activating any other tool
       triggers ``_on_deactivate`` here, so the GUI can clean up its legacy
       state via the optional ``on_deactivated`` callback.

    The tool itself never consumes events — ``handle_click`` / ``handle_motion``
    / ``handle_release`` all return ``False`` so events flow through to the
    legacy handlers.

    Args:
        axes: matplotlib axes the tool is bound to.
        canvas: matplotlib canvas the tool is bound to.
        on_deactivated: optional zero-arg callable invoked whenever the tool is
            deactivated (either explicitly via ``deactivate_tool()`` or because
            ToolManager activated a different tool). GUIs should use this to
            uncheck their legacy buttons / clear ``self.function`` so the UI
            state stays in sync with the tool manager.
    """

    def __init__(self, axes, canvas, on_deactivated=None):
        super().__init__(axes, canvas)
        self.on_deactivated = on_deactivated

    def _on_activate(self):
        """No-op: tool has no visual elements."""
        pass

    def _on_deactivate(self):
        """Fire the optional callback so the GUI can sync legacy state."""
        if self.on_deactivated is not None:
            self.on_deactivated()

    def handle_click(self, event) -> bool:
        """Don't handle click events - let them pass through to the GUI."""
        return False

    def handle_motion(self, event) -> bool:
        """Don't handle motion events - let them pass through to the GUI."""
        return False

    def handle_release(self, event) -> bool:
        """Don't handle release events - let them pass through to the GUI."""
        return False
