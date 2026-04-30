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

from matplotlib.patches import Rectangle
from matplotlib.widgets import RectangleSelector

from .interaction_tool import InteractionTool


class BoxStatsTool(InteractionTool):
    """
    Multi-shot rectangular ROI tool for region intensity statistics with
    per-box re-editing.

    Concepts
    --------
    *Editable box*: at most one rectangle is "live" at a time. It is bound
    to the matplotlib ``RectangleSelector`` so the user can drag corner /
    edge handles to resize and reposition it. Each motion fires
    ``on_box_drawn(bounds, idx)``.

    *Frozen box*: a box that has been committed (via ``commit_current()``)
    and is now a static, numbered rectangle on the axes. Frozen boxes can
    be brought back into edit mode via ``start_editing(bounds, idx)``.

    Workflow
    --------
    The GUI typically wires this up to a list of rows in a popup, each row
    having an Edit/Done button:

    - User drags a new rectangle on the image.
        → ``on_box_drawn(bounds, frozen_count + 1)`` fires; GUI adds a row
          with the button labelled "Done".
    - User clicks Done → ``commit_current()`` is called.
        → Selector is reset, a numbered patch is drawn, and
          ``on_committed(idx, bounds)`` fires. GUI flips the row's button
          to "Edit".
    - User clicks Edit on a frozen row → ``start_editing(bounds, idx)``.
        → Any currently editable box is auto-committed first (so the
          invariant "at most one editable" holds). Then this idx becomes
          the editable one — its frozen patch is removed and the selector
          is positioned over its bounds. ``on_box_drawn(bounds, idx)``
          fires.

    Cleanup
    -------
    When the tool is deactivated for any reason (user uncheck, ESC, image
    change, mutual-exclusion auto-deactivate from ToolManager), all boxes
    (frozen and editable) are removed from the axes, the selector is
    destroyed, and ``on_deactivated`` fires so the GUI can drop its popup.

    Bounds dictionary format
    ------------------------
    ``{'x0': float, 'y0': float, 'x1': float, 'y1': float}`` with
    ``x0 <= x1`` and ``y0 <= y1`` in image (axes data) coordinates.

    Args:
        axes / canvas: matplotlib axes / canvas the tool is bound to.
        on_box_drawn: ``callable(bounds, idx)`` fired on every selector
            update of the editable box. ``idx`` is the index this box
            should be displayed under (for new boxes that's
            ``frozen_count + 1``; for re-edits it's the original idx).
        on_committed: ``callable(idx, bounds)`` fired when
            ``commit_current()`` freezes a box.
        on_deactivated: zero-arg callback fired on ``_on_deactivate``.
    """

    LABEL_FROZEN = "box_stats_frozen"
    LABEL_CURRENT = "box_stats_current"

    # Cycle through these for frozen boxes so overlapping ROIs are easier
    # to tell apart. The editable box always uses CURRENT_COLOR.
    FROZEN_COLORS = ("lime", "cyan", "orange", "magenta", "red")
    CURRENT_COLOR = "yellow"

    def __init__(self, axes, canvas,
                 on_box_drawn=None,
                 on_committed=None,
                 on_deactivated=None):
        super().__init__(axes, canvas)
        self.on_box_drawn = on_box_drawn
        self.on_committed = on_committed
        self.on_deactivated = on_deactivated

        self.selector = None
        self.current_bounds = None
        # Set when re-editing a frozen box; ``None`` while the user is
        # drawing a brand new box (idx will be assigned at commit).
        self.current_idx = None
        # Sequential counter for new boxes. Re-edits do NOT bump this.
        self.frozen_count = 0
        # idx -> [Rectangle, Text] artists drawn on the axes for frozen boxes
        self.frozen_artists = {}

    # ------------------------------------------------------------------ lifecycle
    def _on_activate(self):
        self.current_bounds = None
        self.current_idx = None
        self.frozen_count = 0
        self.frozen_artists = {}
        self._remove_all_artists()
        self._make_selector()

    def _on_deactivate(self):
        self._destroy_selector()
        self._remove_all_artists()
        self.canvas.draw_idle()
        if self.on_deactivated is not None:
            self.on_deactivated()

    # ------------------------------------------------------------------ public API
    def commit_current(self):
        """
        Freeze the currently editable box (if any).

        Returns ``(idx, bounds)`` of the freshly committed box, or ``None``
        if there is nothing to commit. ``on_committed`` fires with the
        same arguments. New draws get ``idx = ++frozen_count``; re-edits
        keep their original idx.
        """
        if self.current_bounds is None:
            return None
        if self.current_idx is None:
            self.frozen_count += 1
            idx = self.frozen_count
        else:
            idx = self.current_idx
        b = dict(self.current_bounds)

        # Replace any stale artists for this idx (re-edit case)
        self._remove_frozen_idx(idx)

        color = self.FROZEN_COLORS[(idx - 1) % len(self.FROZEN_COLORS)]
        rect = Rectangle(
            (b['x0'], b['y0']),
            b['x1'] - b['x0'],
            b['y1'] - b['y0'],
            linewidth=1.2, edgecolor=color, facecolor='none',
            label=self.LABEL_FROZEN,
        )
        self.axes.add_patch(rect)
        text = self.axes.text(
            b['x0'], b['y0'], f" #{idx}",
            color=color, fontsize=9, va='bottom', ha='left',
            label=self.LABEL_FROZEN,
        )
        self.frozen_artists[idx] = [rect, text]

        self._reset_selector()
        self.current_bounds = None
        self.current_idx = None
        self.canvas.draw_idle()

        if self.on_committed is not None:
            self.on_committed(idx, dict(b))
        return (idx, b)

    def start_editing(self, bounds, idx):
        """
        Pull a frozen box back into edit mode.

        Auto-commits any currently editable box first (firing
        ``on_committed`` for it), then removes the frozen artist for
        ``idx``, positions the selector over ``bounds``, and fires
        ``on_box_drawn(bounds, idx)`` so the GUI can flip the row state.
        """
        # First freeze whatever is currently being edited (if any)
        if self.current_bounds is not None:
            self.commit_current()

        # Strip the frozen visual for the box we're re-editing
        self._remove_frozen_idx(idx)

        self.current_idx = idx
        self.current_bounds = dict(bounds)

        if self.selector is not None:
            try:
                self.selector.extents = (
                    bounds['x0'], bounds['x1'], bounds['y0'], bounds['y1'])
                self.selector.set_visible(True)
            except Exception:
                pass

        self.canvas.draw_idle()

        if self.on_box_drawn is not None:
            self.on_box_drawn(dict(bounds), idx)

    def get_result(self):
        """Stats are pushed via callbacks; nothing to pull on deactivate."""
        return None

    # ------------------------------------------------------------------ InteractionTool API
    # All event handlers return ``self.is_active`` so ImageViewerWidget
    # treats us as the active modal interaction (no built-in pan, no
    # canvasClicked emission). RectangleSelector hooks the matplotlib
    # events directly via mpl_connect for the actual drag UX.
    def handle_click(self, event) -> bool:
        return self.is_active

    def handle_motion(self, event) -> bool:
        return self.is_active

    def handle_release(self, event) -> bool:
        return self.is_active

    # ------------------------------------------------------------------ internals
    def _make_selector(self):
        self.selector = RectangleSelector(
            self.axes,
            self._on_select,
            useblit=True,
            button=[1],          # left mouse button only
            minspanx=2, minspany=2,
            spancoords='pixels',
            interactive=True,    # keep handles after release for live editing
            props=dict(
                facecolor=self.CURRENT_COLOR,
                edgecolor=self.CURRENT_COLOR,
                alpha=0.18,
                fill=True,
            ),
            handle_props=dict(
                markeredgecolor=self.CURRENT_COLOR,
                markerfacecolor='white',
                markersize=6,
            ),
        )

    def _destroy_selector(self):
        if self.selector is not None:
            try:
                self.selector.set_active(False)
                self.selector.set_visible(False)
            except Exception:
                pass
            self.selector = None

    def _reset_selector(self):
        """Hide the live selector after a commit; matplotlib re-shows it on next drag."""
        if self.selector is None:
            return
        try:
            self.selector.set_visible(False)
            # Collapse extents so a stale rectangle doesn't flash before the
            # next drag.
            self.selector.extents = (0, 0, 0, 0)
        except Exception:
            pass

    def _on_select(self, eclick, erelease):
        x1, y1 = eclick.xdata, eclick.ydata
        x2, y2 = erelease.xdata, erelease.ydata
        if None in (x1, y1, x2, y2):
            return

        x0, x1n = (x1, x2) if x1 <= x2 else (x2, x1)
        y0, y1n = (y1, y2) if y1 <= y2 else (y2, y1)

        # Ignore degenerate "phantom" selects (single click, no real drag)
        if (x1n - x0) < 1 or (y1n - y0) < 1:
            return

        self.current_bounds = {'x0': x0, 'y0': y0, 'x1': x1n, 'y1': y1n}

        # Re-edits keep their original idx; new draws preview as the
        # next-to-be-assigned idx.
        idx = (self.current_idx
               if self.current_idx is not None
               else self.frozen_count + 1)
        if self.on_box_drawn is not None:
            self.on_box_drawn(dict(self.current_bounds), idx)

    def _remove_frozen_idx(self, idx):
        if idx not in self.frozen_artists:
            return
        for art in self.frozen_artists[idx]:
            try:
                art.remove()
            except Exception:
                pass
        del self.frozen_artists[idx]
        self.canvas.draw_idle()

    def _remove_all_artists(self):
        for arts in list(self.frozen_artists.values()):
            for art in arts:
                try:
                    art.remove()
                except Exception:
                    pass
        self.frozen_artists.clear()
        # Defensive sweep: drop anything else still labelled as ours
        for patch in list(self.axes.patches):
            if patch.get_label() in (self.LABEL_FROZEN, self.LABEL_CURRENT):
                patch.remove()
        for txt in list(self.axes.texts):
            if txt.get_label() in (self.LABEL_FROZEN, self.LABEL_CURRENT):
                txt.remove()
